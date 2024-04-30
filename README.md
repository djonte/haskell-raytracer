# Haskell Raytracer

The goal is to follow the ["Ray Tracing in One Weekend" guide](https://raytracing.github.io/books/RayTracingInOneWeekend.html#outputanimage) by Peter Shirley, Trevor David Black and Steve Hollasch with a twist. I'll make it with Haskell. This poses several challenges as my knowledge in Haskell is severely limited (mostly from IndaPlus {first year in KTH} and DD1366 Programmeringsparadigm, where the latter contains a few projects in the language). Another challenge is to convert the thinking of an imperative solution (in which the tutorial is written) to a functional paradigm. 

To furthermore make the project fairly unique, I will make sure that my images are different scenes. 

# Images generated so far
### A gradient image
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/554df427-f38b-481f-aa6b-00ec998beeeb)
![image](https://github.com/djonte/haskell-raytracer/assets/90456387/da414c36-f2cb-4c7c-8c83-736c679c465e)

### A gradient image generated using Y-pos of ray
![raytracing-gradient](https://github.com/djonte/haskell-raytracer/assets/90456387/6eee13b6-4ce0-46ba-96df-23885d7fb25d)


###

## How to run the Project

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
