# How probabilistic programming can improve statistical literacy

Probabilistic programming (PP) is an incredible tool made possible through advances in modern computing - both in terms of hardware and software. PP enables one to flexibly specify feasibly infinite conceptualisations of statistical models with any number of parameters and estimate their distributional quantities through techniques such as Markov chain Monte Carlo. In this tutorial, we are going to explore the benefits and basics of PP through the language [Stan](https://mc-stan.org), which interfaces easily to both R and Python.

As noted in the title of this tutorial, we are largely going to focus on why PP is important from the lens of pedagogy. In my undergraduate and Masters education in statistics education (through psychology), I was left with so many questions and so many unknowns, mostly due to the lack of mathematical rigour in the courses. One of my lingering feelings turned observations over the past few years has been that even the most commonplace of tools - linear regression - is not well understood by psychology (and other) graduates, and this flows on to impact literacy and understanding of more complex extensions and topics. The remainder of this tutorial will explore basic linear regression conceptually and practically in reproducible code. Note that we will be considering the maximum likelihood estimation of a linear model, not the ordinary least squares approach. This is important, as PP implements likelihood-based approaches (as it is Bayesian in nature), as well as the two differing in their philosophical and mathematical approach to solving the regression problem. If you do not know what I mean by that, fear not, we are going to cover it right now!

## Mathematical formulation of linear regression

Linear regression is typically written in the following familiar form, as taught in every quantitative course at university:

$$y = \Beta_{0} + \Beta x + \epsilon$$

Where y is our estimated mean quantity, $\Beta_{0}$ is the intercept (the expected y value when x is equal to zero), $\Beta$ is the coefficient/weight, x is the observed predictor variable, and $\epsilon$ is Gaussian-distributed noise, parametrised by its mean and standard deviation $N(\mu, \sigma)$. However, what this formulation misses is the whole point of what we are doing when fitting a linear model - estimating a conditional mean that has some error ($\epsilon$) around it. Since we are exploring the maximum likelihood approach to solving it, we can also phrase this as trying to find the values $\mu$ and $\sigma$ which are most likely to have *generated* the data we have observed. The way this is achieved is by maximising the likelihood function with respect to these two values. I won't go into how this is optimisation problem is solved here (spoiler: nonlinear optimisers). This point on generation is very, very important - the core of statistics is estimating quantities we cannot observe or measure, such as an entire population. To reiterate: doing statistical modelling **is not** about trying to produce the correct outputs in your SPSS or R window. It **is** about trying to model and understand the *statistical process* that is most likely to have generated the data you have observed. This framing enables researchers to get at the important questions in science.

With this new conceptualisation in mind, we can rewrite the regression equation as:

$$y ~ Normal(\mu, \sigma)$$

Where $\mu$ is equal to $\Beta_{0} + \Beta x$. In this form it is clear to see that in the ideal world, $\Beta x$ would equal `y`. But since nothing is perfect, we have some error or noise around it. This specification of linear regression is also extremely useful as it enables an easier leap from linear regression to [*generalised linear models*](https://en.wikipedia.org/wiki/Generalized_linear_model) (GLMs). GLMs are another statistical concept that was brushed over in my statistical education, but one that I use daily in my job as a data scientist. GLMs extend linear regression to fit data that is distributed by all manner of non-normal methods. Examples of these distributions include [Poisson](https://en.wikipedia.org/wiki/Poisson_distribution) and [negative binomial](https://en.wikipedia.org/wiki/Negative_binomial_distribution) for count outcome variables, [Bernoulli](https://en.wikipedia.org/wiki/Bernoulli_distribution) for binary outcome variables, [gamma](https://en.wikipedia.org/wiki/Gamma_distribution) for positive continuous outcome variables, and [beta](https://en.wikipedia.org/wiki/beta_distribution) for continuous outcome variables ranged between 0 and 1. In this formulation above, we can flexibly switch out the "Normal" component for any of these other distributions, because a GLM still estimates a conditional mean using a linear predictor (i.e. $\Beta x$), but "connects" this estimation to the target distribution through a [*link function*](https://en.wikipedia.org/wiki/Generalized_linear_model#Probability_distribution), with some error associated with it. See how much more useful this formulation is? The added bonus is that this formulation also sets one up to code in a probabilistic programming language as it encourages a distributional and compositional way of thinking.

Now, since we are using probabilistic programming in this tutorial, we need to get from this point to a Bayesian lens. I won't cover Bayesian theory in too much depth here but I will give a very brief rundown to make the remaining content make sense.

### Bayesian formulation

The primary objective of [Bayesian statistics](https://en.wikipedia.org/wiki/Bayesian_statistics) is to estimate a posterior distribution. The posterior is calculated from the product of two other distributions - a prior distribution and a likelihood distribution and tells us the probability of the model parameters given the data. We already covered the likelihood above - it is the probability of the data *given* the observations and model parameters. The prior is where things get awesome. The prior encodes our existing understanding and expectations *before* seeing the data. Yes, you read that correctly. You can explicitly build in subject matter expertise and your own knowledge into your models. Think of all the papers you have read on a topic, or think about what you think might come out of a model - you almost always have some kind of prior information. In linear regression, the prior is comprised of probability distributions of model parameters. Multiplying the prior and likelihood gives us the posterior*.

*NOTE: The product of the prior and likelihood is standardised against the marginal likelihood (probability of the data) to make the posterior a proper probability distribution.*

## Introduction to Stan and basic formulation

The above description is deceptively simple. Estimating posterior distributions is actually very computationally difficult, especially for complex models or messy data, and it is only due to the benefits of modern computing that Bayesian methods have seen a surge in use. [Stan](https://mc-stan.org) is a probabilistic programming language based on the C++ language and is the leading language for Bayesian implementation. Learning Stan can feel overwhelming at first, especially if you don't know C++, but with pointers (no pun intended) in the right direction, it can also immediately feel so much more empowering than fitting frequentist models in base R or Python does. To quote [Peter Ellis](http://freerangestats.info) the Chief Data Scientist of my firm Nous Group: "Writing models in Stan is the closest thing in statistics to art because it brings you so close to the machine level and lets you specify almost anything." I echo this wholeheartedly. There is just *something* about fully specifying every single thing in a statistical model by hand that stimulates a scientific keenness (as well as confidence in deeply understanding your research question, available information, and model itself).

To start out, let's look at how a linear model is specified in R. We can do this 2 ways, either using an OLS approach through lm() (not the focus of this tutorial) or using a maximum likelihood approach through glm() :

```
m1 <- lm(y ~ x, data = data)
m2 <- glm(y ~ x, data = data, family = gaussian(link = "identity"))
```

Now these approaches are popular for a reason - they are so simple! But what they explicitly miss is the deeper conceptual thought that one should take when fitting a statistical model. The thought of *what is the statistical process we are trying to model and understand?*. These standard approaches are [frequentist](https://en.wikipedia.org/wiki/Frequentist_inference) in nature, an alternative to Bayesian statistics which doesn't formulate estimations as probability distributions. To contrast the simplicity of the base R approaches we can specify the same model as a probabilistic one in Stan with room for flexible specification of a prior on the coefficients we believe we are likely to see (we will ignore the coding conventions of Stan and focus on the statistical implications of the model - see below for a detailed breakdown of this).

Any Stan program is divided into a discrete number of sections. The most basic model will contain 3 parts:
- data (the inputs to our program - e.g. variables)
- parameters (the values we wish to estimate in the model)
- model (the model formulation and any priors we wish to specify)

A Bayesian (probabilistic) formulation of the same models coded above in Stan can be written as:

```
//
// This is a basic Stan program for implementing a simple linear regression model
//

//
// Author: Trent Henderson, 25 May 2021
//

data {
  int<lower=1> N; // Number of observations (sample size)
  vector[N] x; // Predictor variable
  vector[N] y; // Response variable
}

parameters {
  real alpha; // Intercept term to be estimated
  real beta; // Regression coefficient to be estimated
  real<lower=0> sigma; // Standard deviation of our Gaussian likelihood term to be estimated
}

model {
  // Priors

  alpha ~ normal(0,1)
  beta ~ normal(0,1)

  // Likelihood

  y ~ normal(alpha + beta*x, sigma);
}
```

### Data block

This is a process-y block where we tell Stan the various external things we wish to feed into the model. In this case, we are just defining how big the sample size is, and our two variables of interest - x and y - and denoting that they are both vectors with length N. Pretty straightforward.


### Parameters block

This block is where we define any quantities we are wanting to estimate through the model. Evidently, we are trying to estimate an intercept, regression coefficient, and standard deviation that defines the error or uncertainty associated with our mean estimations. We have set a lower bound of 0 for the standard deviation as we cannot have negative values.

### Model block

**This is the most important block for the purposes of this tutorial.** I want to direct your attention to the Likelihood section first. Notice how this is identical to the regression formulation we specified above? This perfectly highlights why this alternative formulation is so important. We now have the theory connecting directly with the code.

The second part of this block is the Priors section. This is where your creativity comes in. I have just specified some trivial means and standard deviations for both for now, but in your own projects, these values would be driven by subject matter expertise, past research, and your expectations of the distributions of coefficients and intercept terms that are most likely to have generated the data you have observed.

*Note that Stan models almost always include a generated quantities block after the model block to account for predictions and model diagnostics. This is beyond the scope of this tutorial.*


## Reflections on the Stan formulation

And there we have it! A fully Bayesian linear regression that adequately tries to model the entire statistical process that underpins our data! Hopefully this conceptual and programming formulation has helped you see why probabilistic programming tools are useful, and how they force you to think deeper about the data at hand and the problem you are trying to solve. The key takeaway is to be aware that specifying models in Stan requires much more thought about what you are trying to do, and this really drives you towards fitting the most appropriate model rather than "fitting what you know" or fitting within the constraints of what's available in one-liner functions.
Future learning

Thanks for making it this far! If you are interested in learning more, I strongly recommend checking out the following excellent resources:
- [Regression and Other Stories](https://avehtari.github.io/ROS-Examples/)
- [Statistical Rethinking](https://xcelab.net/rm/statistical-rethinking/)
- [brms](https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf)
