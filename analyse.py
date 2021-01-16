from getdist import loadMCSamples,plots,covmat
import numpy as np
import os,fnmatch

#for index in range(len(filenames)):
    
#    os.rename("./chains/"+str(filenames[index]),"./chains/mcmc_final_output_"+str(index+1)+".txt")

#number_of_parameters = 10

samples = loadMCSamples('/datos/wilmar.cardona/projects/dea/g5l/MI/chains/mcmc_final_output',settings={'ignore_rows':.0})

g = plots.getSinglePlotter()

g.settings.rcSizes(axes_fontsize = 6,lab_fontsize = 8)

g.settings.x_label_rotation = -45

g.settings.auto_ticks = True

g.settings.legend_fontsize = 11

p = samples.getParams()

bestfit = samples.getLikeStats()

means = samples.setMeans()

stats = samples.getMargeStats()

stats.saveAsText('/datos/wilmar.cardona/projects/dea/g5l/MI/chains/1Dstatistics.txt')

print '1D STATISTICS FILE CREATED'

f = plots.getSubplotPlotter()

f.settings.rcSizes(axes_fontsize = 6,lab_fontsize = 8)

f.settings.legend_fontsize = 8

f.plots_1d(samples)

f.export('/datos/wilmar.cardona/projects/dea/g5l/MI/chains/1D_plots.pdf')

print '1D PLOTS CREATED'

g.triangle_plot(samples,filled=True)

for ax in g.subplots[:,0]:
    ax.axvline(2.218e-2,color='black',ls='--')

for ax in g.subplots[1:,1]:
    ax.axvline(1.205e-1,color='black',ls='--')

for ax in g.subplots[2:,2]:
    ax.axvline(9.619e-1,color='black',ls='--')
    
for ax in g.subplots[3:,3]:
    ax.axvline(3.056,color='black',ls='--')

for ax in g.subplots[4:,4]:
    ax.axvline(66.93,color='black',ls='--')

for ax in g.subplots[5:,5]:
    ax.axvline(6.e-2,color='black',ls='--')

for ax in g.subplots[6:,6]:
    ax.axvline(1.,color='black',ls='--')

for ax in g.subplots[7:,7]:
    ax.axvline(0.,color='black',ls='--')

for ax in g.subplots[8:,8]:
    ax.axvline(-0.8,color='black',ls='--')

for ax in g.subplots[9:,9]:
    ax.axvline(0.,color='black',ls='--')

for ax in g.subplots[9,0:9]:
    ax.axhline(0.,color='black',ls='dotted')

for ax in g.subplots[1,0:1]:
    ax.axhline(1.205e-1,color='black',ls='dotted')

for ax in g.subplots[2,0:2]:
    ax.axhline(9.619e-1,color='black',ls='dotted')

for ax in g.subplots[3,0:3]:
    ax.axhline(3.056,color='black',ls='dotted')

for ax in g.subplots[4,0:4]:
    ax.axhline(66.93,color='black',ls='dotted')

for ax in g.subplots[5,0:5]:
    ax.axhline(6.e-2,color='black',ls='dotted')

for ax in g.subplots[6,0:6]:
    ax.axhline(1.,color='black',ls='dotted')

for ax in g.subplots[7,0:7]:
    ax.axhline(0.,color='black',ls='dotted')

for ax in g.subplots[8,0:8]:
    ax.axhline(-0.8,color='black',ls='dotted')

for ax in g.subplots[9,0:9]:
    ax.axhline(0.,color='black',ls='dotted')

g.export('/datos/wilmar.cardona/projects/dea/g5l/MI/chains/triangle_figure.pdf')

print 'TRIANGLE PLOT CREATED'

exit()

