from nilearn import surface
import trimesh
mesh = trimesh.load('granger_10_subregions.obj')
data_img = image.load_img('granger_10_subregions.nii.gz')
vertices = mesh.vertices   # shape (N_vertices, 3)
faces = mesh.faces         # shape (N_faces, 3)
vertices = np.asarray(vertices, dtype=float)
faces = np.asarray(faces, dtype=np.int32)
mesh_data = (vertices, faces)
surf_values = surface.vol_to_surf(data_img, mesh_data, radius=3.0, interpolation='nearest')

