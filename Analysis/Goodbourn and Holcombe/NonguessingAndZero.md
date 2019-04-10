Goodbourn and Holcome: Proportion of efficacious SPEs = 0
================

<style type="text/css">
.table {

    width: 40%;
    margin-left:auto;
    margin-right:auto;
}
</style>
In the second condition of Experiment 1 in Goodbourn & Holcombe (2015), participants viewed two RSVP streams, one of which was cued. That paper's analyses compare streams based on their spatial location and found no effect of position in this condition. However, we don't care about spatial position in our task, so we need to collapse the streams together and fit a mixture model to the responses from whatever stream was cued on a particular trial. I used the mixRSVP package for this.

Goodbourn and Holcombe's data looked like this for a random participant

<img src="NonguessingAndZero_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

We combine the SPEs into a single, cued, stream. <img src="NonguessingAndZero_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

Parameter Estimates
-------------------

<img src="NonguessingAndZero_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" /><img src="NonguessingAndZero_files/figure-markdown_github/unnamed-chunk-5-2.png" style="display: block; margin: auto;" /><img src="NonguessingAndZero_files/figure-markdown_github/unnamed-chunk-5-3.png" style="display: block; margin: auto;" />

| Parameter |   Mean|     Sd|
|:----------|------:|------:|
| efficacy  |   0.72|   0.16|
| latency   |  39.30|  21.73|
| precision |  72.98|  24.11|

Proportion of non-guessing distribution corresponding to SPE = 0
----------------------------------------------------------------

Here are the proportions of the nonguessing distributions from the model fits that corresponded to an SPE of 0.

| Participant |    SPEZero| Participant |    SPEZero|
|:------------|----------:|:------------|----------:|
| EL          |  0.4475125| JH          |  0.3291070|
| FJ          |  0.3114947| JI          |  0.3929549|
| LS          |  0.4461880| JJ          |  0.4567148|
| PG          |  0.3641494| JK          |  0.2017215|
| SY          |  0.3506111| JL          |  0.5789603|
| WC          |  0.3767794| JM          |  0.4718886|
| JA          |  0.5058569| JN          |  0.3852124|
| JB          |  0.3712831| JO          |  0.4479977|
| JC          |  0.6987403| JP          |  0.3479885|
| JD          |  0.3790081| JQ          |  0.5479431|
| JE          |  0.5633382| JR          |  0.4762714|
| JF          |  0.4806241| JS          |  0.5764781|
| JG          |  0.5077556| JT          |  0.3982198|

All of these are less than one, which is what we want. A t-test confirms this

    ## 
    ##  One Sample t-test
    ## 
    ## data:  params$pBetween
    ## t = -27.409, df = 25, p-value < 2.2e-16
    ## alternative hypothesis: true mean is less than 1
    ## 95 percent confidence interval:
    ##       -Inf 0.4739903
    ## sample estimates:
    ## mean of x 
    ## 0.4390308

And here are the nonguessing distributions, with SPE = 0 highlighted

    ##        mean        sd
    ## 1 0.4390308 0.1043586

<img src="NonguessingAndZero_files/figure-markdown_github/unnamed-chunk-9-1.png" width="100%" style="display: block; margin: auto;" />

This confirms that the SPE = 0 measure is a conservative measure of efficacy. Efficacious trials are spread over several SPEs, and SPE = 0 only corresponds to a subset of these trials.

Bibliography
============

Goodbourn, P. T., & Holcombe, A. O. (2015). “Pseudoextinction”: Asymmetries in simultaneous attentional selection. *Journal of Experimental Psychology: Human Perception and Performance*, *41*(2), 364.
