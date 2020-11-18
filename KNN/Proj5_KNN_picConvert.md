```python
from matplotlib import pyplot as io 
import numpy as np
from PIL import Image
import random
```


```python
# Image to array
img1 = io.imread('Koala.jpg').copy() #image is saved as rows * columns * 3 array print (img1)
img2 = io.imread('Penguins.jpg').copy()
```


```python
np.shape(img1)

```




    (768, 1024, 3)




```python
np.shape(img2)
```




    (768, 1024, 3)




```python
random.seed(31)
```


```python
def randxy():
    i=random.randint(0,767)
    j=random.randint(0,1023)
    x=[i,j]
    return x
```


```python
def pick(k):
    x=0
    locations=[]
    while x<k:
        locations.append(randxy())
        x=x+1
    return locations
```


```python
img1[245,439,0]
```




    204




```python
img1[535,710]
```




    array([117, 118, 110], dtype=uint8)




```python
def getKmean(k):
    x = pick(k)
    kpos = np.array(x)
    return kpos
```


```python
np.shape(getKmean(3))[0]
```




    3




```python
img1[getKmean(3)[0][0],getKmean(3)[0][1]]
```




    array([118,  99,  69], dtype=uint8)




```python
def getRGB(kpos,img):
    rgb={}
    for i in range(np.shape(kpos)[0]): 
        rgb[i]=np.array(img[kpos[i][0],kpos[i][1]])
    return rgb
```


```python
x1=getRGB(getKmean(3),img1)
print(x1)
print(x1[0])
```

    {0: array([151, 129, 105], dtype=uint8), 1: array([70, 70, 62], dtype=uint8), 2: array([147, 140, 132], dtype=uint8)}
    [151 129 105]



```python
def KNN(iteration,img,k):
    iterate = 0
    kmeans = getKmean(k)
    rgbMean = getRGB(kmeans,img)
    while iterate < iteration:
        iterate = iterate + 1
        for x in range(np.shape(img)[0]):
            for y in range(np.shape(img)[1]):
                dist = {}
                for z in range(np.shape(kmeans)[0]):
                    a=img[x][y]
                    b=rgbMean[z]
                    dist[z] = ((a[0]-b[0])**2)+((a[1]-b[1])**2)+((a[2]-b[2]**2))
                minkey = min(dist, key=dist.get) #find the closest mean 
#                 print(rgbMean[minkey])
                img[x][y] = rgbMean[minkey]#replace color
    return img
```


```python
newimg1 = KNN(1,img1,4)
```

    /Users/wenxionglu/opt/anaconda3/lib/python3.7/site-packages/ipykernel_launcher.py:13: RuntimeWarning: overflow encountered in ubyte_scalars
      del sys.path[0]



```python
np.shape(newimg1)
```




    (768, 1024, 3)




```python
ni1 = Image.fromarray(newimg1) 
ni1.save('testrg.png')
```


