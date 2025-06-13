library(gifti)
library(fsbrain)

###### main
file_lh = 'cortisol_BrainThickness_zcoef_sub19_hemi-L.gii'
file_rh = 'cortisol_BrainThickness_zcoef_sub19_hemi-R.gii'
lh_gii <- read_gifti(file_lh)
rh_gii <- read_gifti(file_rh)

#colFn_blue_red = colorRampPalette(c("#013e7d", "#FFFFFF", "#760715")); red blue
#colFn_wbview = colorRampPalette(rev(c("#ffff00", "#ffc800", "#ff7800", '#ff0000',
#                                      '#c80000', '#960000', '#640000', '#3c0000',
#                                      '#000050', '#0000aa', '#4b007d', '#7d00a0', 
#                                      '#4b7d00', '#00c800', '#00ff00', '#00ffff')));
## The above color is from WorkBench
colFn_wbview = colorRampPalette
fsbrain.set.default.figsize(1200, 1200);

lh_data = lh_gii$data$unknown[,1]
#lh_data[lh_data<100] = NA
rh_data = rh_gii$data$unknown[,1]
#rh_data[rh_data<100] = NA

coloredmeshes <- vis.data.on.fsaverage(
  vis_subject_id = "fsaverage",
  morph_data_lh = lh_data,
  morph_data_rh = rh_data,
  surface = "pial",
  views = c("t4"),
  rgloptions=rglo(),
  rglactions = list('trans_fun'=limit_fun(-0.6,0.6)), # limit values
  draw_colorbar = 'horizontal',
  makecmap_options = list('colFn'=colFn_wbview, symm=FALSE, col.na='white'),
  bg = 'sulc',
  morph_data_both = NULL,
  style = "default"
)

#export(coloredmeshes, colorbar_legend = "sulcal depth [mm]", transparency_color = "#FFFFFF");
