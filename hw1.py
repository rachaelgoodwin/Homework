import numpy as np
import statsmodels.formula.api as smf
import pandas as pd
import matplotlib.pyplot as plt

#random sample  of normal distribution
n=1000
x=np.random.normal(0,1,n)
y=10+5*x+np.random.normal(0,1,n)
reg_data=pd.DataFrame({'x':x,'y':y})

#fitting model to the data and printing it
regmodel=smf.ols(formula='y~x',data=reg_data)
fittedmodel=regmodel.fit()
print(fittedmodel.summary())

#plotting the data vs the model
from statsmodels.graphics.regressionplots import abline_plot
ax=reg_data.plot(x='x',y='y',kind='scatter')
abline_plot(model_results=fittedmodel,ax=ax,color='red')
plt.show()

#reading in hibbs data and sorting it
hibbs=pd.read_table('C:\\Users\\racha\\Downloads\\hibbs.dat',header=0,sep=" ")
print(sorted(hibbs))

#fitting ols model to the data and plotting it 
ln=smf.ols(formula='vote~growth',data=hibbs)
lnfit=ln.fit()
print(lnfit.summary())
hibbsgraph=hibbs.plot(x='growth',y='vote',kind='scatter')
abline_plot(model_results=lnfit,ax=hibbsgraph)
plt.show()

#plotting snow data
snow=pd.read_csv('C:\\Users\\racha\\Downloads\\snow.csv')
print(sum(snow['prob'][0:16]))
snow.plot(kind='scatter',x='snowfall',y='prob')

#calculating the cumulative sum for each observation of the snow data
plt.show()
snow['csum']=[0]*len(snow['prob'])
for i in range(0,len(snow['prob'])-1):
    print(i)
    snow['csum'][i]=sum(snow['prob'][0:i])

#plotting the cumulative sum
snow.plot(kind='scatter',x='snowfall',y='csum')
plt.show()
#calculating mean,median,mode and variance of the snow data 
print(sum(snow['snowfall']*snow['prob']))
print(sum(snow['snowfall']**2*snow['prob'])-(sum(snow['snowfall']*snow['prob']))**2)
print(snow['snowfall'][(pd.Series.idxmax(snow['prob']))])
prob=0
for i in range(0,len(snow['prob'])-1):
    if prob<0.5:
        prob=prob+snow['prob'][i]
    else:
        print(snow['snowfall'][i-1])
        break
print('median',prob)
#calculating the 95th percentile snowfall
prob=0
for i in range(0,len(snow['prob'])-1):
    if prob<0.95:
        prob=prob+snow['prob'][i]
    else:
        print(snow['snowfall'][i-1])
        break
print('95',prob)

#calculating the log odds and creating a confidence interval for the mean snowfall
print(snow['prob'][0]/(1-snow['prob'][0]))
mean=sum(snow['snowfall']*snow['prob'])
var=sum(snow['snowfall']**2*snow['prob'])-(sum(snow['snowfall']*snow['prob']))**2
print(mean-1.96*var**0.5,mean+1.96*var**0.5)

#reading in data, calculating the marginal distribution for x
income=pd.read_csv('C:\\Users\\racha\\Downloads\\income-savings.csv')
y= list(income)
xmarginal=[0]*( len(income.loc[0]))
y.remove('Unnamed: 0')
x=list(income[('Unnamed: 0')])
income.iloc[len(income['Unnamed: 0'])-1]=income.sum(axis='index',numeric_only=True)
print(income.iloc[len(income['Unnamed: 0'])-1])

#calculating the marginal distribution for y
income['ymarginal']=income[y].sum(axis='columns',numeric_only=True)
print(income['ymarginal'])

#calculating the conditional expectation y|X and the unconditional expectation E[Y] 
cond=pd.read_csv('C:\\Users\\racha\\Downloads\\income-savings.csv')
condexp=[0]*income.iloc[len(income['Unnamed: 0'])-1]
e_y=[0]
for i in range(1,len(income.iloc[0])-1):
    for j in range(0,len(income['Unnamed: 0'])-1):
        cond.iloc[j,i]=income.iloc[j,i]/income.iloc[len(income['Unnamed: 0'])-1,i]
        condexp[i]=condexp[i]+cond.iloc[j,i]*income.iloc[j,0]
    e_y=e_y+condexp[i]*income.iloc[len(income['Unnamed: 0'])-1,i]
print(cond)
print(condexp)
print(e_y)

#calculating E[Y] with the marginal distribution
y1 = np.array([0.5,0.4,0.25,0.15,0.05,0.0,-0.05,-0.18,-0.25])
e_y_y=y1@income.iloc[0:9,11].transpose()
print(e_y_y)

#calculating E[X],E[XY] and E[X^2]
x2 = np.array([ .5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.7, 8.8, 12.5, 17.5])
e_x=x2@income.iloc[9,1:11].transpose()
e_x2=x2**2@income.iloc[9,1:11].transpose()
e_xy = y1 @ income.iloc[0:9,1:11] @ x2.transpose()

#calculating the best linear predictor
cov=e_xy-e_x*e_y
beta=cov/(e_x2-e_x**2)
alpha=e_y-beta*e_x
print(beta,alpha)