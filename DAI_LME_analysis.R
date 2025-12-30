library(dplyr)
library(tidyr)
library(data.table)
library(afex)
library(emmeans)
library(broom.mixed)

df_org = fread('granger_analysis_10_subregions_revise.csv')
df = data.table()  
df$Subject = df_org$subject 
df$Condition = df_org$run
df$Condition[df_org$run == 1] = 'RW'
df$Condition[df_org$run == 2] = 'SD'
df$Region = df_org$type
df$Direction = df_org$direct
df$delta_R2 = df_org$delta_R2

# 建议：让 afex 使用 lmerTest 提供 p 值
afex_options(method_mixed = "S")  # Satterthwaite df
# 或者：afex_options(method_mixed = "KR")  # Kenward-Roger（更慢）


# 1) 从原始 df 计算 DAI
dai_df <- df %>%
  mutate(
    Subject   = factor(Subject),
    Condition = factor(Condition, levels = c("RW","SD")),
    Region    = factor(Region),
    Direction = factor(Direction), 
  ) %>%
  # 若同一 Subject×Condition×Region×Direction 有重复，先取均值
  #group_by(Subject, Condition, Region, Direction) %>%
  #summarise(delta_R2 = mean(delta_R2, na.rm = TRUE), .groups = "drop") %>%
  # pivot 成 L->R / R->L 两列
  pivot_wider(names_from = Direction, values_from = delta_R2) %>%
  # 计算 DAI = (L->R) - (R->L)
  mutate(DAI = `top` - `down`) %>%
  select(Subject, Condition, Region, DAI)


# 2) 全局 LME：检验 Condition × Region 交互（你要的“哪些脑区变化不一样”的前提）
m_global <- mixed(
  DAI ~ Condition * Region + (1 | Subject),
  data = dai_df,
  method = "S",     # Satterthwaite；也可用 "KR"
  type = 3          # Type-III tests
)

m_global

# 3) 事后对比：每个 Region 内做 SD − RW 的差（LME 框架下），并做 BH-FDR
# 取每个 Region 内 Condition 的边际均值
emm <- emmeans(m_global$full_model, ~ Condition | Region)

# 计算 SD - RW（因为你设置 levels=c("RW","SD")，所以 SD-RW 就是第二个-第一个）
#contr <- contrast(emm, method = "revpairwise")  # 等价于 SD - RW（更直观）
# 如果你想更明确：
contr <- contrast(emm, method = list("SD_minus_RW" = c(-1, 1)))

contr_df <- as.data.frame(contr)

# BH-FDR across regions
contr_df$q_fdr <- p.adjust(contr_df$p.value, method = "BH")
contr_df$sig_fdr <- contr_df$q_fdr < 0.05

contr_df

# 4)（强烈推荐）补充“方向反转”判据：RW 和 SD 的平均 DAI 符号是否相反
mean_sign <- dai_df %>%
  group_by(Region, Condition) %>%
  summarise(mean_DAI = mean(DAI, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Condition, values_from = mean_DAI) %>%
  mutate(mean_sign_flip = sign(RW) * sign(SD) == -1)

# 合并到对比结果里
final_res <- contr_df %>%
  rename(Region = Region) %>%
  left_join(mean_sign, by = "Region") %>%
  arrange(q_fdr, p.value)

final_res

# plot
summary_sign <- dai_df %>%
  group_by(Region, Condition) %>%
  summarise(
    mean_DAI = mean(DAI, na.rm = TRUE),
    sd_DAI   = sd(DAI, na.rm = TRUE),
    n        = sum(!is.na(DAI)),
    se_DAI   = sd_DAI / sqrt(n),
    .groups = "drop"
  )

sign_info <- summary_sign %>%
  mutate(
    sign_mean = sign(mean_DAI)
  ) %>%
  select(Region, Condition, sign_mean) %>%
  pivot_wider(
    names_from  = Condition,
    values_from = sign_mean
  ) %>%
  mutate(
    mean_sign_flip = sign(RW) * sign(SD) == -1
  )

summary_sign_full <- summary_sign %>%
  left_join(sign_info %>% select(Region, mean_sign_flip),
            by = "Region")

summary_sign_full <- summary_sign_full %>%
  mutate(
    Condition = factor(Condition, levels = c("RW", "SD")),
    Region    = factor(Region, 
                       levels = c('dlPu','vmPu','PPtha','cHipp','GP','Stha','dCa','cTtha','Otha','vCa')), 
  )
library(ggplot2)


ggplot( summary_sign_full, aes(x = Condition, y = mean_DAI, group = Region) ) +
  # y = 0 baseline
  #geom_hline(yintercept = 0, color = "grey50", linewidth = 0.6) +
  # 斜线：连接 RW → SD（每个 Region 一条）
  geom_line(color = "black", linewidth = 0.6 ) +
  # error bar
  geom_errorbar(aes(ymin = mean_DAI - se_DAI, ymax = mean_DAI + se_DAI ),
    width = 0.15, linewidth = 0.6 ) +
  # point
  geom_point(aes( shape = Condition, color = mean_DAI > 0 ),
    size = 10, stroke = 0.8 ) +
  scale_shape_manual( values = c("RW" = 16, "SD" = 17) ) +
  scale_color_manual( 
    values = c("TRUE"  = "red", "FALSE" = "blue"), 
    labels = c("FALSE" = "DAI < 0", "TRUE" = "DAI > 0")
  ) +
  facet_wrap(~ Region, nrow = 1) +
  labs( x = "", y = "DAI", shape = "Condition", color = "DAI sign" ) +
  theme_test() + 
  theme(
    legend.position = 'none', 
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 24),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1), 
    plot.title = element_text(size = 32, hjust = 0.1), 
    strip.text = element_text(size = 32))  
