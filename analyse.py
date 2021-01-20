from getdist import loadMCSamples,plots,covmat
import numpy as np
import os,fnmatch

#for index in range(len(filenames)):
    
#    os.rename("./chains/"+str(filenames[index]),"./chains/mcmc_final_output_"+str(index+1)+".txt")

#number_of_parameters = 10

samples = loadMCSamples('/home/wcardona/projects/deaMI/g5l/mcmc/chains/mcmc_final_output',settings={'ignore_rows':.0})

g = plots.getSinglePlotter()

g.settings.rcSizes(axes_fontsize = 6,lab_fontsize = 8)

g.settings.x_label_rotation = -45

g.settings.auto_ticks = True

g.settings.legend_fontsize = 11

p = samples.getParams()

bestfit = samples.getLikeStats()

means = samples.setMeans()

stats = samples.getMargeStats()

stats.saveAsText('/home/wcardona/projects/deaMI/g5l/mcmc/output/1Dstatistics.txt')

print '1D STATISTICS FILE CREATED'

f = plots.getSubplotPlotter()

f.settings.rcSizes(axes_fontsize = 6,lab_fontsize = 8)

f.settings.legend_fontsize = 8

f.plots_1d(samples)

f.export('/home/wcardona/projects/deaMI/g5l/mcmc/figures/1D_plots.pdf')

print '1D PLOTS CREATED'

g.triangle_plot(samples,filled=True)

for ax in g.subplots[:,0]:
    ax.axvline(2.248e-2,color='black',ls='--')

for ax in g.subplots[1:,1]:
    ax.axvline(1.189e-1,color='black',ls='--')

for ax in g.subplots[2:,2]:
    ax.axvline(9.677e-1,color='black',ls='--')
    
for ax in g.subplots[3:,3]:
    ax.axvline(3.041,color='black',ls='--')

for ax in g.subplots[4:,4]:
    ax.axvline(69.43,color='black',ls='--')

for ax in g.subplots[5:,5]:
    ax.axvline(8.7e-2,color='black',ls='--')

for ax in g.subplots[6:,6]:
    ax.axvline(1.,color='black',ls='--')

for ax in g.subplots[7:,7]:
    ax.axvline(-0.56,color='black',ls='--')

for ax in g.subplots[8:,8]:
    ax.axvline(-1.1,color='black',ls='--')

for ax in g.subplots[9:,9]:
    ax.axvline(0.0527,color='black',ls='--')

for ax in g.subplots[10:,10]:
    ax.axvline(9.6e-3,color='black',ls='--')

for ax in g.subplots[1,0:1]:
    ax.axhline(1.189e-1,color='black',ls='dotted')

for ax in g.subplots[2,0:2]:
    ax.axhline(9.677e-1,color='black',ls='dotted')

for ax in g.subplots[3,0:3]:
    ax.axhline(3.041,color='black',ls='dotted')

for ax in g.subplots[4,0:4]:
    ax.axhline(69.43,color='black',ls='dotted')

for ax in g.subplots[5,0:5]:
    ax.axhline(8.7e-2,color='black',ls='dotted')

for ax in g.subplots[6,0:6]:
    ax.axhline(1.,color='black',ls='dotted')

for ax in g.subplots[7,0:7]:
    ax.axhline(-0.56,color='black',ls='dotted')

for ax in g.subplots[8,0:8]:
    ax.axhline(-1.1,color='black',ls='dotted')

for ax in g.subplots[9,0:9]:
    ax.axhline(0.0527,color='black',ls='dotted')

for ax in g.subplots[10,0:10]:
    ax.axhline(9.6e-3,color='black',ls='dotted')

g.export('/home/wcardona/projects/deaMI/g5l/mcmc/figures/triangle_figure.pdf')

print 'TRIANGLE PLOT CREATED'

exit()

