Welcome
-------

Welcome to the tutorial! In this short piece we are going to explore the
motivation for writing functions in C and C++ for use in R and take a
look at two quick benchmarking examples. Full reproducible R and C++
code will be presented throughout.

Why C and C++?
--------------

C and C++ are low-level languages that have been around for decades and
underpin a staggeringly wide range of software that we all know and use
day-to-day. Even R itself is written in C! One of the primary reasons
for their success is their efficiency. C and C++ are compiled languages,
and are much closer to machine code than high-level languages such as R.
This means they can run computations far, far faster than high-level
languages, but require a lot of detailed coding knowledge and skills in
order to write these programs. While the barrier to entry is much
higher, particularly if you come from high-level languages or no
computer science education, the payoffs for applied work can be immense,
particularly with regards to efficiency.

The volume and size of datasets being analysed by data scientists,
consultants, and other analysts today are getting increasingly larger.
As a result, computations on these datasets can take a long time. With
this in mind, programmers need to be able to write functions that can
scale to meet the efficiency needs of these modern datasets. C and C++
are two languages that can help provide this need.

We are going to take a look at two examples of rewriting R code in C++
and comparing their relative computation time.

Examples
--------

### Example 1: Rescaling function

Rescaling is a common technique used prior to statistical and machine
learning modelling as it puts all the variables on the same playing
field. This means techniques that could be biased by large variances,
such as the gradient descent algorithm used in neural networks, can
account for this beforehand. Typically (but not always), people would
rescale into three potential ranges:

-   0 to 1
-   -1 to +1
-   Normalised (accounts for mean and standard deviation)

We are going to take a look at the first today, though the code
presented can extend to any of them as we will discuss.

#### The simple, readily available R function

We can explore the comparative performance of C++ by writing a small
function and benchmarking the speed of computation over multiple
automated calculations. While most introductory examples for R users
learning C and C++ usually involve writing a simple program to compute a
sum or a mean, we are going to look at something more practical: a
function to rescale a vector of numbers into a user defined range. The
sum/mean/median functions and other similar ones are already quite
highly optimised, so rewriting them into C++ would not afford much gain,
aside from a handle on the basics. We can examine the existing R
function in the below code, where we will rescale the numeric vector
into the range of `[0,1]`:

    library(scales)

    # Generate some synthetic data

    x <- seq(from = 1, to = 1000, by = 1)

    # Rescale it

    scales::rescale(x, to = c(0,1))

But how does this function work? Essentially, the mathematics under the
hood looks like this (where `x` is the value we want to rescale and `i`
denotes the index in the vector):

$$x\_{new} = \\frac{new\_{max}-new\_{min}}{old\_{max}-old\_{min}}+(x\_{i}-old\_{max})+new\_{max}$$

But what if we were required to rescale hundreds of thousands of values
at once? What if we had to do this across hundreds of columns in a
dataframe? Potentially writing an equivalent function in C or C++ might
afford us large performance gains as dataset size scales. This is
because the overhead of operations such as for loops is much less in
these languages compared to R. R is a *vectorised* language, meaning
these types of operations are much faster than for loops as R stores the
entire loop at each iteration.

#### C++ version

We can now directly translate that mathematical equation into C++ using
the code below (integration with R made simple by the fantastic `Rcpp`
package). Note the key differences to most standard R functions:

-   **We need to specify the data type for every variable and
    argument.** R does a lot of high level things for us, such as
    converting integers to doubles when we want to do math on both types
    at once. C++ does not do this, so we must declare every variable
    type explicitly. This means the code is more verbose, but is much
    clearer to the interested reader. An example in the code below is
    denoting the input and output vectors as `NumericVector`.
-   **Operations are accessed with a period.** Similar to Python, C++
    using periods to denote an operation. This can be seen in the usage
    of `x.size();` where we calculate the *length* or size of the input
    vector. This length is then used to drive the for loops so that we
    can do the same operation on all entries in the vector.
-   **Arguments are ended with a semicolon.** This is extremely
    important and is the source of many error messages for R users
    learning C++. You must end lines of code that do things with a `;`.
-   **For loop notation is different and all indexing starts at 0.**
    This is very very critical: Indexing starts at 0 in C and C++, not 1
    like it does in R. Remember this! Further, the loop syntax is also
    different. We need to declare the type of `i` that we are looping
    over, and then specify the operator `++i` to increment to *new*
    values of `i`.
-   **Comments are written with double forward slashes.** Comments in C
    and C++ are typically denoted by `//` instead of `#` used in R.

Taking a step back, examining the math equation and my C++ code
side-by-side,

*NOTE: This rescaling function we called `cpp_scaler` is the
`normalise_catch` function I wrote for my R package
[`catchEmAll`](https://github.com/hendersontrent/catchEmAll).*

    library(Rcpp)

    # Write the C++ function

    cppFunction('
    NumericVector cpp_scaler(NumericVector x) {

      int n = x.size();
      double old_min = 0.0;
      double old_max = 0.0;
      double new_min = 0.0;
      double new_max = 1.0;
      NumericVector x_new(n);

      // Calculate min

      for (int i = 0;  i < n; ++i){
        if (x[i] < old_min){
          old_min = x[i];
        }
      }

      // Calculate max

      for (int i = 0; i < n; ++i){
        if (x[i] > old_max){
          old_max = x[i];
        }
      }

      // Rescale into sigmoidal range

      for (int i = 0;  i < n; ++i){
        x_new[i] = ((new_max-new_min)/(old_max-old_min))*(x[i]-old_max)+new_max;
      }
      return x_new;
    }
    ')

If we wanted to further improve the flexibility of our C++ function, we
could pass in the `new_min` and `new_max` components as arguments to the
overall function to allow the user to pick the range they want to
rescale into. Further, in a actual project, we would store the C++
functions in separate `.cpp` files and call them to R using `sourceCpp`.
This is consistent with modular programming.

To test our function’s performance, we can benchmark the performance
against the `rescale` function we used in the R package `scales` using
the `microbenchmark` package.

    # Generate some synthetic data

    x <- seq(from = 1, to = 1000, by = 1)

    # Benchmark

    microbenchmark::microbenchmark(cpp_scaler(x), scales::rescale(x, to = c(0,1)), times = 1000)

    ## Unit: microseconds
    ##                              expr    min     lq     mean  median      uq      max neval
    ##                     cpp_scaler(x)  3.827  6.040 18.03594  7.7315  8.9865 9192.121  1000
    ##  scales::rescale(x, to = c(0, 1)) 32.844 44.093 51.21626 50.7425 56.3315  140.323  1000

Clearly, our C++ function is much faster at calculating the rescaled
values over the 1000 calculations (most easily seen in the `mean` column
for the 1000 tests). Now let’s make a larger vector sampled from a
probability distribution and compare the performance:

    # Generate some synthetic data

    x1 <- rnorm(n = 1e5, mean = 5, sd = 2)

    # Benchmark

    microbenchmark::microbenchmark(cpp_scaler(x1), scales::rescale(x1, to = c(0,1)), times = 1000)

    ## Unit: microseconds
    ##                               expr      min        lq      mean    median        uq      max neval
    ##                     cpp_scaler(x1)  255.069  383.4695  690.7072  606.6505  657.0925  5750.15  1000
    ##  scales::rescale(x1, to = c(0, 1)) 1040.689 1901.0330 2801.6784 2442.1605 2656.3330 99308.63  1000

The results clearly speak for themselves in this example. Let’s take a
look at one more.

### Example 2: Calculating entropy of a signal

There is an important subfield of mathematics known as [information
theory](https://en.wikipedia.org/wiki/Information_theory) which deals
with calculations associated with transmitting signals over noisy
channels. Information theory has shown immense utility in a variety of
applications, with some notable examples including data compression
(e.g. MP3, ZIP files), message encoding, and understanding how complex
networks such as the brain and financial markets transmit information.
In information theory, *information* is defined as a *reduction in
uncertainty/surprise*. By extension, this means that high probability
events communicate *less* information than low probability events,
because we learn more about a system from the occurrence of a low
probability event.

One of the core quantities in information theory is [Shannon
entropy](https://en.wikipedia.org/wiki/Entropy_(information_theory)),
which is the average reduction in uncertainty of a given variable.
Shannon entropy is measured in *bits* (when using log base 2) and is
mathematically defined as:

$$H(X) = -\\sum\_{i=1}^{n}P(x\_{i})log\_{2}P(x\_{i})$$

Enough theory, let’s code up a function from scratch in both R and C++
this time and compare their performance on a simulated vector of values.
Since Shannon entropy works with values taken from a [probability mass
function](https://en.wikipedia.org/wiki/Probability_mass_function), we
are going to feed in a vector of probabilities we are calling `X`.

#### R function

    r_entropy <- function(X){
      
      H_of_X <- 0
      
      for(i in seq_along(X)){
        H_of_X <- i*log2(i)
      }
      
      H_of_X <- -H_of_X
      return(H_of_X)
    }

#### C++ function

This time I will show you the C++ code as if I wrote it in a dedicated
`.cpp` file. Note the use of `#include` headers which give us access to
certain operators (think of these as like R packages or libraries for
C++).

    #include <Rcpp.h>
    #include <math.h>
    using namespace Rcpp;

    // [[Rcpp::export]]
    double cpp_entropy(NumericVector X) {
      
      int n = X.size();
      double H_of_X = 0.0;
      
      for(int i = 0; i < n; ++i){
        H_of_X += X[i]*log2(X[i]);
      }

      return -H_of_X;
    }

#### Performance comparison

    library(dplyr) # For dataframe wrangling
    library(magrittr) # For the '%>%' operator

    # Generate some synthetic probabilities

    x2 <- data.frame(x = rpois(1000, lambda = 3)) %>% # Simulate some integers
      mutate(x = x / sum(x)) # Computes probabilities that sum to 1

    # Benchmark

    microbenchmark::microbenchmark(cpp_entropy(x2$x), r_entropy(x2$x), times = 1000)

    ## Unit: microseconds
    ##               expr     min       lq       mean   median       uq      max neval
    ##  cpp_entropy(x2$x)   5.945   7.1575   9.255549   7.6395   8.4080  851.514  1000
    ##    r_entropy(x2$x) 203.223 243.4075 300.568206 251.9775 285.3255 4560.251  1000

Another clear win for C++! The order of magnitude difference in speed
between them is staggering.

Final thoughts
--------------

C and C++ are fantastic languages that have stood the test of time for a
multitude of reasons. While programming in them takes time and a
relatively steep learning curve, I firmly believe it is worth the
investment, particularly if you want a better grasp on the core ideas of
general programming and/or want faster functions that scale well to
large-scale problems. However, do be aware that learning these languages
is tricky, and coding more complex algorithms and functions can be very
tedious. But if you are likely to reuse functions a lot or need to scale
to meet the computational demands of large datasets, investing the time
in coding things in C/C++ could be well worth it - especially if you
turn your functions into an R package!

Some other resources that I recommend to get started writing code in
these languages (especially for use in R) are:

-   [Rewriting R code in C++](https://adv-r.hadley.nz/rcpp.html) - from
    the master Hadley Wickham
-   [Seamless R and C++ Integration with
    Rcpp](http://www.rcpp.org/book/) - from the author of the `Rcpp`
    package Dirk Eddelbuettel
-   [Effective Modern
    C++](https://www.oreilly.com/library/view/effective-modern-c/9781491908419/) -
    by Scott Meyers
