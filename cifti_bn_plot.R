##############################################
library(ciftiTools)
ciftiTools.setOption('wb_path', '/usr/local/workbench')
file = '/home/clancy/TemplateFlow/BN_Atlas_freesurfer/100307/fsaverage_LR32k/100307.BN_Atlas.32k_fs_LR.dlabel.nii'
bn = read_cifti(file)
bn_lh = bn$data$cortex_left[,1]
bn_rh = bn$data$cortex_right[,1]
bn_rh_new = bn_rh - 210
bn_rh_new[bn_rh_new<0] = 0
bn_both = c(bn_lh, bn_rh_new)
##############################################
indicator = 'degree'
select_ind = c(indicator, 'participant_id', 'region')
select_func = func_vertex_146[all_group=='patient_before', ..select_ind]
df_average = merge(brainnetome_surface, select_func[, .(Value=mean(degree)), by=.(region)], 
                   by.x = 'name', by.y = 'region')
setkey(df_average, index)
empty_num = rep(0, length(bn_both))
input_data = df_average$Value # 210 values
for (i in seq(210)){
  empty_num[bn_both==i] <- input_data[i]
}
label_xifti <- newdata_xifti(bn, empty_num)
view_xifti_surface(label_xifti, zlim=c(0,75), color_mode='diverging')
##############################################
## make one mask
# Extract the labels
selected_area <- ifelse(label_cifti$data == 10, 1, NA)
view_xifti_surface(
  xifti = label_cifti,          # Original surface
  overlay = selected_area,       # Mask for the selected region
  border_colors = "black",       # Set border color to black
  color_mode = "qualitative"     # Ensure categorical colormap
)

## make multiple mask
overlay_mask <- rep(NA, length(label_cifti$data))  # Initialize empty overlay
# Assign 1 to label 10 and 2 to label 20
overlay_mask[label_cifti$data == 10] <- 1  
overlay_mask[label_cifti$data == 20] <- 2  
view_xifti_surface(
  xifti = label_cifti,       
  overlay = overlay_mask,      
  border_colors = c("black", "red"),  # Black for area 1, Red for area 2
  color_mode = "qualitative"
)
border_colors = "black"
view_xifti_surface(label_cifti, overlay = overlay_mask, border_colors = c("black", "red"), overlay_alpha = 0.8)
