## Test environments

  * local OS X install: 
    * R version 3.3.1 Patched (2016-07-16 r70928)
    * Platform: x86_64-apple-darwin13.4.0 (64-bit)
    * Running under: OS X El Capitan (10.11.6)
  
  * win-builder via devtools

## R CMD check results

There were no ERRORs or WARNINGs

I believe these to be correctly spelled:

  Possibly mis-spelled words in DESCRIPTION:
    Antidifferentiation (4:9)
    antidifferentiation (8:5)


## Downstream dependencies

This package separates out some code from the `mosaic` package related to calculus.  Since this is 
a first CRAN submission, nothing depends on this package yet, but two packages that depend on 
`mosaic` will need to be updated to depend on `mosaicCalc` as well.  These are `statisticalModeling` 
and `fastR`.  These are begin updated simultaneously and will be submitted ASAP after this is on
CRAN.

## Other notes

The title and description have been expanded as per your request.
