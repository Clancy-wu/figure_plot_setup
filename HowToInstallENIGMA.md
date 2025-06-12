# How to install ENIGMA

git clone https://github.com/MICA-MNI/ENIGMA.git  
cd ENIGMA  
conda create --name enigma python=3.8  
conda activate enigma  
pip install numpy  
pip install scipy  
pip install scikit-learn  
pip install matplotlib  
pip install vtk=9.3.1 # vtk should exactly be 9.3.1  
pip install nibabel  
pip install nilearn  
pip install pillow  
pip install pandas  

python setup.py install

Author@kangwu
