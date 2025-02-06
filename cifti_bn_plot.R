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
