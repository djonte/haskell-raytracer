# Haskell Raytracer

The goal is to follow the ["Ray Tracing in One Weekend" guide](https://raytracing.github.io/books/RayTracingInOneWeekend.html#outputanimage) by Peter Shirley, Trevor David Black and Steve Hollasch with a twist. I'll make it with Haskell. This poses several challenges as my knowledge in Haskell is severely limited (mostly from IndaPlus {first year in KTH} and DD1366 Programmeringsparadigm, where the latter contains a few projects in the language). Another challenge is to convert the thinking of an imperative solution (in which the tutorial is written) to a functional paradigm. 

To furthermore make the project fairly unique, I will make sure that my images are different scenes. 

# Images generated so far
### A gradient image
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/554df427-f38b-481f-aa6b-00ec998beeeb)
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/da414c36-f2cb-4c7c-8c83-736c679c465e)

### A gradient image generated using y-pos of ray
![raytracing-gradient](https://github.com/djonte/haskell-raytracer/assets/90456387/6eee13b6-4ce0-46ba-96df-23885d7fb25d)


### A circle generated using ray tracing against hardcoded sphere
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/bf2aef8f-4df1-414e-8b76-bd9fe61d1779)

### Visualizing sphere's outward normals
#### Red as a function of x
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/8e917853-7fc7-4cee-8424-f33c95a47a4e)

#### Green as a function of y
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/5fa3b322-2121-4e05-b4a9-702094005749)

#### Blue as a function of z
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/67e114a3-e36c-448c-802c-29db4b617b44)

Comment: *It should be noted that it is not quite possible to see any substantial change in color in the sphere with the human eye, however upon analysis with Paint it is noticable as a slight decrease in the amount of blue moving towards the edges of the picture, in accordance with expectations. To make this easier to demonstrate I made a modification where I let the blue and green be dependant on the z coordinate of the normalized outer normals of the sphere raised to the power 8. This increases the color change per z coordinate change drastically, see below.*
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/8390661f-f8e1-466e-9be4-ef2f16751918)


#### Red, green and blue as a function of x, y and z respectively
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/47f05f95-d692-4b4a-aed8-203e97d7213a)


#### 2 spheres, both with green and blue as a function of y
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/ef5828dc-4f2a-42fc-8f53-7519c19aedee)

### World and antialiasing
#### Sphere with visualized normals, and ground
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/2a8d1978-1f15-4b73-8e80-c38a53620430)

#### Above but with antialiasing (see edges)
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/bdceb2ed-b9a9-42a8-a5e7-8eb83036be9a)


## How to run the project

1. **Clone the repository**

   Open your terminal and run the following command:

   ```bash
   git clone https://github.com/djonte/haskell-raytracer.git
   ```

2. **Navigate to the project directory**
    ```bash
    cd haskell-raytracer
    ```

3. **Install the dependancies**
    ```bash
    stack install
    ```

5. **Build and run the project**
    ```bash
    stack run
    ```



# Implementation
The solution will be implemented using Haskell.
### Resolver
lts-20.26
### Image generation
Haskell Image Processing Library (HIP)
### 3d vectors
Linear
