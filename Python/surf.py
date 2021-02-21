# This code gives an example of how a surface plot video can be made using outputted simulation data.
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from scipy.io import FortranFile
import glob
import os

def getdata(filenumber, variable, DIR = ' '):

    if DIR == ' ':
        filename = 'Data/' + variable + '{:03d}'.format(filenumber) + '.dat'
    else:
        filename = DIR + variable + '{:03d}'.format(filenumber) + '.dat'

    file = FortranFile(filename, 'r')
    nx = file.read_ints(np.int32)[0]
    ny = file.read_ints(np.int32)[0]
    nz = file.read_ints(np.int32)[0]
    return file.read_reals(float).reshape((nz,ny,nx), order="C")

def getgrid(DIR = ' '):

	global t_max, dt, nx, ny, nz, xb, yb, zb, xc, yc, zc, \
           alpha, rho_bcc, rho_bbb, rho_ccb, rho_cbc

	if DIR == ' ':
		filename = 'Data/grid.dat'
	else:
		filename = DIR + 'grid.dat'

	file = FortranFile(filename, 'r')

	t_max = file.read_reals(float)[0]
	dt = file.read_reals(float)[0]
	nx = file.read_ints(np.int32)[0]
	ny = file.read_ints(np.int32)[0]
	nz = file.read_ints(np.int32)[0]
	xb = file.read_reals(float)
	yb = file.read_reals(float)
	zb = file.read_reals(float)
	xc = file.read_reals(float)
	yc = file.read_reals(float)
	zc = file.read_reals(float)
	alpha = file.read_reals(float)[0]
	rho_bcc = file.read_reals(float).reshape((nz+2,ny+2,nx+1), order="F")
	rho_bbb = file.read_reals(float).reshape((nz+2,ny+2,nx+1), order="F")
	rho_ccb = file.read_reals(float).reshape((nz+2,ny+2,nx), order="F")
	rho_cbc = file.read_reals(float).reshape((nz+2,ny+2,nx), order="F")

def get_xyz(variable, DIR = ' '):

	global x, y, z

	if DIR == ' ':
		getgrid()
	else:
		getgrid(DIR)

	if variable == 'ux1':
		x = xb
		y = yc
		z = zc
	elif variable == 'ux2':
		x = xb
		y = yb
		z = zb
	elif variable == 'u_perp1':
		x = xc
		y = yc
		z = zb
	elif variable == 'u_perp2':
		x = xc
		y = yb
		z = zc
	elif variable == 'bx1':
		x = xb
		y = yc
		z = zb
	elif variable == 'bx2':
		x = xb
		y = yb
		z = zc
	elif variable == 'b_perp1':
		x = xc
		y = yc
		z = zc
	elif variable == 'b_perp2':
		x = xc
		y = yb
		z = zb
	elif variable == 'b_par1':
		x = xc
		y = yc
		z = zc
	elif variable == 'b_par2':
		x = xc
		y = yb
		z = zb
	else:
		print('Error')

def getstep(filenumber, DIR = ' '):
	if DIR == ' ':
		filename = 'Data/step' '{:03d}'.format(filenumber) + '.dat'
	else:
		filename = DIR + 'step' + '{:03d}'.format(filenumber) + '.dat'
	file = FortranFile(filename, 'r')
	return file.read_ints(np.int32)[0]

directory = 'Data/'
getgrid(directory)
var = input('Enter variable name: ')
# var = 'u_perp1'
file_number = len(glob.glob('Data/' + var + '*'))
iy0 = 0

output_dir = 'Figures/Surface/' + var + '/'
os.makedirs(output_dir, exist_ok=True)

get_xyz(var)

for n in range(file_number):

	if n % 100 == 0: print(n)

	time = getstep(n) * dt
	var1 = getdata(n, var)
	if n == 0:
		max_var1 = np.max(var1)
		min_var1 = np.max(var1)
	else:
		max_var1 = np.max([max_var1, np.max(var1)])
		min_var1 = np.min([min_var1, np.min(var1)])

print("max_var1 = " + str(max_var1))
print("min_var1 = " + str(min_var1))

fig = plt.figure()

for n in range(file_number):

	time = getstep(n) * dt

	ax = fig.add_subplot(111, projection='3d')

	get_xyz('u_perp1')
	X, Z = np.meshgrid(x, z)
	var1 = getdata(n, var)
	surf = ax.plot_surface(X, Z, var1[:,iy0,:], cmap=cm.cool)
	ax.set_zlim([min_var1, max_var1])
	surf.set_clim([min_var1, max_var1])
	ax.set_xlabel('x')
	ax.set_ylabel('z')
	ax.set_title(var + ', t = ' + '{:.2f}'.format(time))

	fig.savefig(output_dir + '/' + '{:03d}'.format(n) + '.png', bbox_inches='tight')
	fig.clf()
