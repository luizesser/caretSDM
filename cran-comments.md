## R CMD check results

0 errors | 0 warnings | 4 notes

* This is a new release.
* R CMD check takes 35m 43.5s

June 19th 2025. I have addressed Uwe Ligges comments on the R CMD Check as follows. The licence was 
changed to conform the permissions. Check returned "SDM" and "geoprocessing" as possible
misspelled words in DESCRIPTION, but it is not the case. Both words are very informative of
package goals. I excluded a link for a function in train_sdm documentation that addressed a function
from other package and included the package anchor. Some internal functions were marked as
undocumented objects, due to a @export tag. The tag was removed from these functions. I managed to
reduce examples' execution time. Test times were also reduced. Check total time is taking less than
10 minutes as requested.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* R CMD check takes 6m 2.6s

June 20th 2025. "SDM" and "geoprocessing" words in DESCRIPTION were changed. I excluded "library" 
calls and exluded test-write due to problems with temporary directories creation in CRAN, as well as 
snapshot tests. I reduced even further the examples to keep them in the requested time. I used 
skip_on_cran function to avoid errors with snapshot test.

## R CMD check results

0 errors | 0 warnings | 2 note

* This is a new release.
* R CMD check takes 5m 39.2s

June 21st 2025. I adapted some tests to avoid the use of some bigger files. This saved us the 
required amount of space to keep the package below 5MB.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* R CMD check takes 6m 8.8s
