import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np

with open("output_file") as f:
    ## Reads the output file
    rawdata = f.readlines()

## The x-coordinate of the particle's postion
x = np.array([row.split()[0] for row in rawdata], 'float')
## The y-coordinate of the particle's postion
y = np.array([row.split()[1] for row in rawdata], 'float')
## The z-coordinate of the particle's postion
z = np.array([row.split()[2] for row in rawdata], 'float')
## The x-coordinate of the particle's velocity
vx = np.array([row.split()[3] for row in rawdata], 'float')
## The y-coordinate of the particle's velocity
vy = np.array([row.split()[4] for row in rawdata], 'float')
## The z-coordinate of the particle's velocity
vz = np.array([row.split()[5] for row in rawdata], 'float')
## The kinetic energy of the particle
energy = np.array([row.split()[6] for row in rawdata], 'float')
## The time at current time step
time = np.array([row.split()[7] for row in rawdata], 'float')
               
# Make a new figure twice as long as it is high
fig = plt.figure(figsize=plt.figaspect(0.3))
# First subplot, particle position, in 3D
ax = fig.add_subplot(1, 3, 1, projection='3d')
ax.plot(x, y, z)
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('z')
# Second subplot, particle energy
ax = fig.add_subplot(1, 3, 2)
#rel_energy = np.array([(energy[i] - energy[0])/energy[i] for i in energy],'float')
ax.plot(time, energy)
ax.set_ylim(bottom=0)
ax.set_ylabel('$\Delta$ Energy')
ax.set_xlabel('Time')
# Third subplot, particle x position against time
ax = fig.add_subplot(1, 3, 3)
ax.plot(time, x)
#ax.set_ylim(bottom=0)
ax.set_ylabel('x-position')
ax.set_xlabel('Time')

plt.show()
