# Tables & Graphs


<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->


This report displays the results for two Growth Curve Mixture models with two latent classes from OCTO-Twin and Newcastle 85+ studies,


<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 



<!-- Load the sources.  Suppress the output when loading sources. --> 



<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 


<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


<!-- Load the datasets.   -->














<!-- print results   -->
#Table of Results


effect               label                                              dense                dense                dense                dense               
-------------------  -------------------------------------------------  -------------------  -------------------  -------------------  --------------------
                                                                        Newcastle 85+        Newcastle 85+        OCTO-Twin            OCTO-Twin           
                                                                        Fast decliners       Slow decliners       Fast decliners       Slow decliners      
                                                                        (N=  291, 73%)       (N=  107, 27%)       (N=  386, 68%)       (N=  184, 32%)      
Fixed effects        Intercept                                          25.31( .66), <.001   10.44(2.96), <.001   26.33(1.51), <.001   21.10(10.64),   .05 
Fixed effects        Female                                             -.01( .35),   .98    -2.53(1.37),   .06   .32( .77),   .68     -.73(2.67),   .78   
Fixed effects        Dementia                                           -3.64( .80), <.001   -6.37(1.80), <.001   -3.81(1.01), <.001   -5.96(1.82),   .00  
Fixed effects        Education                                          .27( .13),   .04     -.08( .61),   .89    .17( .29),   .54     -.11(4.33),   .98   
Fixed effects        Years to death from study entry                    .42( .14),   .00     2.51( .66), <.001    .19( .06),   .00     .66( .26),   .01    
Fixed effects        Higher Social Class                                -.33( .39),   .39    1.77(1.38),   .20    .04( .26),   .87     -.61(5.79),   .92   
Fixed effects        Age at study entry                                 -.28( .45),   .53    -.39(1.50),   .79    -.25( .06), <.001    -.48( .36),   .18   
Fixed effects        Linear slope                                       -.54( .35),   .12    -2.66(1.76),   .13   -.77( .84),   .36    -2.17(4.98),   .66  
Fixed effects        Linear slope *Female                               -.21( .21),   .30    -.09(1.04),   .93    .08( .63),   .89     -.02(2.03),   .99   
Fixed effects        Linear slope *Dementia                             -.18( .44),   .68    -1.62( .98),   .10   -1.63( .54),   .00   -1.03( .45),   .02  
Fixed effects        Linear slope *Education                            .07( .07),   .37     -.22( .32),   .49    .04( .15),   .82     -.20(1.11),   .85   
Fixed effects        Linear slope *Years to death from study entry      -.07( .08),   .35    -.53( .32),   .09    .03( .03),   .38     -.17( .16),   .29   
Fixed effects        Linear slope *Social class                         -.31( .23),   .18    -.44( .83),   .59    .04( .10),   .68     -.24(1.11),   .82   
Fixed effects        Linear slope* age at study entry                   .09( .24),   .71     .66( .95),   .49     -.04( .06),   .54    -.03( .17),   .86   
Fixed effects        Quadratic slope                                                                              -.10( .05),   .03    -.16( .48),   .74   
Fixed effects        Quadratic slope *Female                                                                      .01( .08),   .90     -.01( .40),   .98   
Fixed effects        Quadratic slope *Dementia                                                                    -.15( .05),   .00    -.06( .06),   .37   
Fixed effects        Quadratic slope *Education                                                                   .00( .02),   .85     -.02( .06),   .76   
Fixed effects        Quadratic slope *Years to death from study entry                                             -.00( .00),   .44    -.03( .02),   .21   
Fixed effects        Quadratic slope *Social class                                                                .01( .01),   .51     -.04( .04),   .32   
Fixed effects        Quadratic slope* age at study entry                                                          -.00( .01),   .76    -.00( .01),   .95   
Residual variances   Intercept                                          3.70( .80), <.001    3.70( .80), <.001    4.86(1.48),   .00    4.86(1.48),   .00   
Residual variances   Linear slope                                       .32( .23),   .16     .32( .23),   .16     .39( .54),   .47     .39( .54),   .47    
Residual variances   Quadratic slope                                                                              .00( .01),   .66     .00( .01),   .66    
Residual variances   Error                                              1.47( .38), <.001    22.24(4.01), <.001   1.30( .57),   .02    25.83(5.96), <.001  

#Figure 1

<img src="figure_rmd/print-fig-1-1.png" width="800px" />


##By Gender
<img src="figure_rmd/print-fig-1-grouped-1.png" width="800px" />

##By SES

<img src="figure_rmd/print-fig-1-grouped-2.png" width="800px" />

##By Education

<img src="figure_rmd/print-fig-1-grouped-3.png" width="800px" />

##By Dementia

<img src="figure_rmd/print-fig-1-grouped-4.png" width="800px" />

#Figure 2

<img src="figure_rmd/print-fig-2-1.png" width="600px" />



