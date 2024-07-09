library(ggplot2)
# correlation plot
## note: use title bold, 36 size text and 24 size axis for a good view.
data.table(
  indicator=indicator_sub_all,
  beta=colMeans(active_sub_all[,-1])) %>%
  ggplot(., aes(x=indicator, y=beta)) +
    geom_point(size=4) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linewidth=4) +
    theme_test() + 
    labs(x='E.nodal.wt (thres 0.25)', y='Beta', 
         title='Spearman r(274)=-0.452') +
    theme(axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          axis.text = element_text(size=24),
        plot.title = element_text(size = 32, hjust = 0.5, face='bold'))  

color_code <- c("#1F77B4E5", "#FF7F0EE5", "#2CA02CE5", "#D62728E5", "#9467BDE5", "#8C564BE5", "#E377C2E5", "#7F7F7FE5", "#BCBD22E5", "#17BECFE5", "#AEC7E8E5")
color_name <- c( 'R_IPL',    'R_preSMA',     'R_FEF',     'R_IFJ',      'R_Fp',      'R_aMFG', 'R_pMFG',        'R_M1',   'L_aMFG',     'L_pMFG',    'L_Fp')
color_index <- c(1,2,3,4,5,6,7,8,9,10,11)

library(ggplot2)
library(magrittr)
library(extrafont)
font_import()  
loadfonts(device = "pdf")  
fonts()

# no sub-1039 L/R aMFG, (795, 279)
basic_info = read.csv('/home/clancy/Desktop/tmsfmri/tms_first_comp_coeff_df.csv')
basic_info$site <- factor(basic_info$site, levels = color_name )
all_subs = unique(basic_info$subject)


basic_info$sit_num = 1
site_number = as.vector(tapply(basic_info$sit_num, basic_info$subject, sum))
as.data.frame(table(site_number)) %>%
  ggplot(., aes(x=site_number, y=Freq)) +
  geom_bar(stat = 'identity', color='#406AF2', fill='#98C8F2', size=1.5) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x='Sites Number', y='Subjects Number') +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size=24),
    legend.position = 'None')
# 500, 600 ,svg

as.data.frame(table(basic_info$site)) %>%
  ggplot(., aes(x=Var1, y=Freq)) +
  geom_bar(stat = 'identity', color='#406AF2', fill='#98C8F2', size=1.5) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x='', y='Subjects Number') +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size=24)) +
  coord_flip()
# 
library(dplyr)
tms_raw = read.csv('/home/clancy/Desktop/tmsfmri/data_give_zhuoran/tms_data_raw_activation.csv')
tms_first = read.csv('/home/clancy/Desktop/tmsfmri/tms_first_comp_coeff_df.csv')

my_matrix = matrix(nrow=11, ncol=11)
colnames(my_matrix) <- color_name
rownames(my_matrix) <- color_name
for (i in color_name){
  for (k in color_name){
    # i is row and k is col
    subs_r <- vector()
    for (sub in all_subs){
      sub_data_1 = filter(tms_raw, subject==sub & site==i)
      sub_data_1_first = filter(tms_first, subject==sub & site==i)
      sub_data_1_spec = sub_data_1[1, 3:276] - sub_data_1_first[1, 5:278]
      sub_data_2 = filter(tms_raw, subject==sub & site==k)
      sub_data_2_first = filter(tms_first, subject==sub & site==k)
      sub_data_2_spec = sub_data_2[1, 3:276] - sub_data_2_first[1, 5:278]
      if (dim(sub_data_1)[1]==1 & dim(sub_data_2)[1]==1){
        result_cor = cor.test(as.numeric(sub_data_1_spec), as.numeric(sub_data_2_spec))
        subs_r = c(subs_r, result_cor$estimate[[1]])
      }else{
        subs_r = c(subs_r, NA)
      }
    }
    my_matrix[i,k] <- mean(subs_r, na.rm = T)
  }
}
#write.csv(my_matrix, 'spatial_spec_CorMatrix.csv')

library(corrplot)
pdf("corrplot.pdf", family = "Times New Roman")

my_matrix[my_matrix==1] = 0
corrplot(my_matrix, is.corr = F, method = 'color', type = 'lower', col=rev(COL2('RdBu', 100)),
         tl.col = 'black', tl.cex = 1.3, cl.ratio = 0.2, tl.srt = 30 , col.lim=c(-0.4, 0.4),
        tl.pos = 'l',  cl.pos = 'n')

dev.off()


spatial_comps <- read.csv('/home/clancy/Desktop/tmsfmri/spatial_pca_comps.csv')
ggplot(spatial_comps, aes(x=comp, y=value*100)) + 
  geom_violin(trim=F, color='#406AF2', fill='#98C8F2', size=1.5) +
  geom_boxplot(width=0.05) + 
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 80)) +
  #scale_x_discrete(labels = c("comp1" = "1", "comp2" = "2", "comp3" = "3", "comp4" = "4")) +
  labs(x='', y='Explained Ratio (%)') +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size=24),
    legend.position = 'None')

###

teompora_comps <- read.csv('/home/clancy/Desktop/tmsfmri/model_sub_pca_topfour_explanation.csv')
melt(teompora_comps) %>%
ggplot(., aes(x=variable, y=value*100)) + 
  geom_violin(trim=F, color='#406AF2', fill='#98C8F2', size=1.5) +
  geom_boxplot(width=0.05) + 
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 80)) +
  #scale_x_discrete(labels = c("comp1" = "1", "comp2" = "2", "comp3" = "3", "comp4" = "4")) +
  labs(x='', y='Explained Ratio (%)') +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size=24),
    legend.position = 'None')



model_free_dir = '/home/clancy/Desktop/tmsfmri/model_tms_data_raw_activation_subs82'
model_first_dir = '/home/clancy/Desktop/tmsfmri/model_tms_data_first_activation_subs82'
all_files = list.files(model_free_dir, full.names = T)
library(R.matlab)

my_matrix = matrix(nrow=11, ncol=11)
colnames(my_matrix) <- color_name
rownames(my_matrix) <- color_name
my_matrix_p = my_matrix

for ( i in color_name){
  for (k in color_name){
    subs_r <- vector()
    for (sub in all_subs){
      sub_full_file = list.files(model_free_dir, pattern = sub, full.names = T)
      sub_full_data = readMat(sub_full_file)
      # first
      sub_first_file = list.files(model_first_dir, pattern = sub, full.names = T)
      sub_first_data = readMat(sub_first_file)
      i_site =  sprintf(paste0("%-", 7, "s"), gsub('_','', i))
      k_site = sprintf(paste0("%-", 7, "s"), gsub('_','', k))
      sub_i_index = which( i_site == sub_full_data$site[,1])
      sub_k_index = which( k_site == sub_full_data$site[,1])
      if (length(sub_i_index)==1 & length(sub_k_index)==1){
        sub_i_data = sub_full_data$data[sub_i_index, , ] # 161, 274
        sub_k_data = sub_full_data$data[sub_k_index, , ] # 161, 274
        sub_i_first = sub_first_data$data[sub_i_index, , ]
        sub_k_first = sub_first_data$data[sub_k_index, , ]
        # spec
        sub_i_spec = sub_i_data - sub_i_first
        sub_k_spec = sub_k_data - sub_k_first
        r_roi = vector()
        for (j in seq(274)){
          roi_result = cor.test(sub_i_spec[,j], sub_k_spec[,j])
          r_roi = c(r_roi, roi_result$estimate[[1]])
        }
        subs_r = c(subs_r, mean(r_roi, na.rm=T))
      }else{
        subs_r = c(subs_r, NA)
      }
    }
    my_matrix[i, k] <- mean(subs_r, na.rm=T)
  }
}
#write.csv(my_matrix, 'temporal_spec_CorMatrix.csv')
#temporal_spec = read.csv('temporal_spec_CorMatrix.csv')
#temporal_raw = read.csv('temporal_raw_CorMatrix.csv')
spatial_raw = read.csv('spatial_raw_CorMatrix.csv')
spatial_spec = read.csv('spatial_spec_CorMatrix.csv')

df_to_matrix <- function(df){
  df_matrix = df[,-1]
  rownames(df_matrix) <- df[,1]
  return(as.matrix(df_matrix))
}
spatial_raw_matrix = df_to_matrix(spatial_raw)
spatial_spec_matrix = df_to_matrix(spatial_spec)
A = spatial_raw_matrix # upper
B = spatial_spec_matrix # lower
# merge
merged_matrix <- matrix(0, nrow = 11, ncol = 11)
colnames(merged_matrix) <- color_name
rownames(merged_matrix) <- color_name

merged_matrix[upper.tri(merged_matrix, diag = TRUE)] <- A[upper.tri(A, diag = TRUE)]
merged_matrix[lower.tri(merged_matrix, diag = TRUE)] <- B[lower.tri(B, diag = TRUE)]

library(corrplot)
pdf("spatial_raw_spec.pdf", family = "Times New Roman")
merged_matrix[merged_matrix==1] = 0

corrplot(merged_matrix, is.corr = F, method = 'color', col=rev(COL2('RdBu', 100)),
         tl.col = 'black', tl.cex = 1.3, cl.ratio = 0.2, col.lim=c(-0.3, 0.3))

dev.off()

####
activation_sig = '/home/clancy/Desktop/tmsfmri/spatial_common_effect_info_brain125.csv'
graph_atlas = '/home/clancy/Desktop/tmsfmri/graph_theory/network_construct/ba_274_info.csv'
sig_info = read.csv(activation_sig)
atlas_info = read.csv(graph_atlas)

library(data.table)
View(merge(sig_info, atlas_info, by.x = 'roi_index', by.y = 'index'))

sig_full_info = sig_info %>%
  filter(roi_index<210) %>%
  merge(., atlas_info, by.x = 'roi_index', by.y = 'index')

pdf("Nightingale rose diagram.pdf", family = "Times New Roman")
# begin
sig_info_withcerebellum <- sig_info %>%
  merge(., atlas_info, by.x = 'roi_index', by.y = 'index')
library(viridis)
dat <- as.data.frame(table(sig_info_withcerebellum$lobe))
p <- ggplot(dat) +
  geom_bar(aes(x=Var1, y=Freq, fill=Var1), stat='identity', size=0.6) +
  scale_fill_viridis(discrete=TRUE, direction = 1, option = "C", name="") +
  ylim(0,40) + # solve the problem of insufficient display
  coord_polar() +
  labs(x='', y='')+
theme(
  panel.background = element_rect(fill = "white", colour = "white"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "right", # no legend
  legend.text = element_text(size=12), #large font for website display
  legend.title = element_text(size=10), #large font for website display
  axis.text = element_blank(),
  xis.title = element_blank() )
# Add labels on top of each bar
label_data <- dat
angle= round(90 - 360 * (c(1:nrow(label_data))-0.5) /nrow(label_data),0)
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)
p + geom_text(data=label_data, aes(x=Var1, y=Freq+0.5, label=Var1, hjust=hjust),
              color="black", size=6, angle= label_data$angle, inherit.aes = FALSE)
# end
dev.off()

sum(label_data$Freq) # 125
label_data$prop <- label_data$Freq / 125 * 100
label_data

####
library(fsbrain)
library(RColorBrewer)
library(freesurferformats)
num_from_label <- function(data_index, data_value, label_file){
  # data is 210
  label_info = read.fs.annot(label_file)
  label_vertex = label_info$label_codes
  label_code = label_info$colortable_df$code
  label_index = label_info$colortable_df$struct_index
  empty_num = rep(0, length(label_vertex))
  for (i in seq(data_index)){
    i_code = label_code[label_index == data_index[i]]
    empty_num[label_vertex == i_code] = data_value[i]
  }
  empty_num[empty_num==0] = NA
  return(empty_num)
}

##############
# 0.05, 0.3
sig_pos <- sig_full_info %>%
  arrange(desc(tvalue)) %>%
  slice_head(n=10)

# -0.45, -0.25
"sig_neg <- sig_full_info %>%
  arrange(tvalue) %>%
  slice_head(n=10)"
#colFn_diverging_pos = function(n) { grDevices::hcl.colors(n, palette = "YlOrRd"); }
# colFn_diverging_neg = function(n) { grDevices::hcl.colors(n, palette = "GnBu"); }
#colFn_diverging_neg = function(n) { grDevices::hcl.colors(n, palette = "TealGrn",rev=T); }
#blue_to_green <- function(n) { colorRampPalette(c("#00FE80", "#0003BF"))(n) }

sig_neg <- sig_full_info %>%
  arrange(tvalue) %>%
  slice_head(n=10)
sig_data = sig_neg
blue_to_green <- function(n) { colorRampPalette(c("#00FE80", "#0003BF", 'red', 'yellow'))(n) }
rgla = list('trans_fun'=limit_fun(-0.45, 0.45)) # limit values
#rgla = list('trans_fun'=limit_fun(0.05, 0.3)) # limit values
label_lh_file = '/home/clancy/Pictures/BN_Atlas_freesurfer/fsaverage/label/lh.BN_Atlas.annot'
label_rh_file = '/home/clancy/Pictures/BN_Atlas_freesurfer/fsaverage/label/rh.BN_Atlas.annot'
lh_num = num_from_label(sig_data$roi_index, sig_data$zvalue,label_lh_file)
rh_num = num_from_label(sig_data$roi_index, sig_data$zvalue,label_rh_file)
coloredmeshes <- vis.data.on.fsaverage(
  vis_subject_id = "fsaverage",
  morph_data_lh = lh_num,
  morph_data_rh = rh_num,
  surface = "pial",
  views = c("t4"),
  draw_colorbar = 'horizontal',
  makecmap_options = list('colFn'=blue_to_green,symm=T, 'col.na'='white', 'range'=c(-0.45,0.45)),
  bg = 'curv_light',
  morph_data_both = NULL,
  style = "default"
)

export(coloredmeshes, colorbar_legend='', output_img='sig_colorbar.png', draw_colorbar = T);

####
sig_neg <- sig_full_info %>%
  arrange(tvalue) %>%
  slice_head(n=10)
sig_info_withcerebellum  = sig_neg
# begin
pdf("neg_sig_nanding.pdf", family = "Times New Roman")
library(viridis)
dat <- as.data.frame(table(sig_info_withcerebellum$lobe))
p <- ggplot(dat) +
  geom_bar(aes(x=Var1, y=Freq), stat='identity', color='#406AF2', fill='#98C8F2',size=0.6) +
  #scale_fill_viridis(discrete=TRUE, direction = 1, option = "C", name="") +
  ylim(0,7) + # solve the problem of insufficient display
  coord_polar() +
  labs(x='', y='')+
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "n", # no legend
    #legend.text = element_text(size=12), #large font for website display
    #legend.title = element_text(size=10), #large font for website display
    axis.text = element_blank(),
    xis.title = element_blank() )
# Add labels on top of each bar
label_data <- dat
angle= round(90 - 360 * (c(1:nrow(label_data))-0.5) /nrow(label_data),0)
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)
p + geom_text(data=label_data, aes(x=Var1, y=Freq+0.5, label=Var1, hjust=hjust),
              color="black", size=6, angle= label_data$angle, inherit.aes = FALSE)
# end
dev.off()


sig_pos <- sig_full_info %>%
  arrange(desc(tvalue)) %>%
  slice_head(n=10)
sig_info_withcerebellum  = sig_pos
dat <- as.data.frame(table(sig_info_withcerebellum$lobe))
dat

###### network
blue_to_green <- function(n) {
  colorRampPalette(c("#00FE80", "#0003BF", 'red', 'yellow'))(n)
}

nework_index = 7
network_df = filter(sig_full_info, network==nework_index) # -0.45~0.45
colFn_diverging = function(n) { grDevices::hcl.colors(n, palette = "Blue-Red 3"); }
label_lh_file = '/home/clancy/Pictures/BN_Atlas_freesurfer/fsaverage/label/lh.BN_Atlas.annot'
label_rh_file = '/home/clancy/Pictures/BN_Atlas_freesurfer/fsaverage/label/rh.BN_Atlas.annot'
lh_num = num_from_label(network_df$roi_index, network_df$zvalue,label_lh_file)
rh_num = num_from_label(network_df$roi_index, network_df$zvalue,label_rh_file)
coloredmeshes <- vis.data.on.fsaverage(
  vis_subject_id = "fsaverage",
  morph_data_lh = lh_num,
  morph_data_rh = rh_num,
  surface = "pial",
  views = c("t4"),
  draw_colorbar = 'horizontal',
  makecmap_options = list('colFn'=blue_to_green, symm=T, 'col.na'='white', 'range'=c(-0.45,0.45)),
  bg = 'curv_light',
  morph_data_both = NULL,
  style = "default"
)
export(coloredmeshes, colorbar_legend='', output_img='network_7.png',draw_colorbar=F);




model_free_dir = '/home/clancy/Desktop/tmsfmri/model_tms_data_raw_activation_subs82'
model_files = list.files(model_free_dir, full.names = T)
file_1 = model_files[1]
file_data = readMat(file_1)

brain_index = c(2,9,84,75,132,133,164,165,177,180,192,201,234,225,253)
brain_sig_index = c(68,85,155,170,182,209,238,252)
  
aa = cor(file_data$data[1, , ][, brain_sig_index])
write.table(aa, file='/home/clancy/Desktop/tmsfmri/pictures/edge_example_sigbrain.edge', sep=' ', row.names = F, col.names = F)

## graph theory
library(data.table)
graph_274 <- fread('/home/clancy/Desktop/tmsfmri/graph_theory/rsfmri_group_raw_brain274_vertex.csv')
graph_125 <- fread('/home/clancy/Desktop/tmsfmri/graph_theory/rsfmri_group_raw_brain125_vertex.csv')
activation_sig = fread('/home/clancy/Desktop/tmsfmri/spatial_common_effect_info_brain125.csv')
tms_first = fread('/home/clancy/Desktop/tmsfmri/tms_first_comp_coeff_df.csv')

tms_first_avg <- as.vector(colMeans(tms_first[, 5:278]))
graph_274_avg <- graph_274[threshold==0.25, c(E.nodal.wt)]

df <- data.table(Beta = tms_first_avg, Topo = graph_274_avg)
cor.test(df$Beta, df$Topo)
# 274
ggplot(df, aes(x=Topo, y=Beta)) +
  geom_point(size=8, shape=21, fill='#98C8F2',colour='#98C8F2', alpha=.5) +
  geom_smooth(method = "lm", se = TRUE,  fill='#406AF2', linewidth=4 ) +
  theme_test() + 
  labs(x='', y='', 
       title='') +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text = element_text(size=30) )  
