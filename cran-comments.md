## Responses to the prevous submission
* Kurt Hornik writes:

The new version still has the same problem.  You have

R> tools:::showNonASCIIfile("NAMESPACE")
3: exportPattern("<cb><86>[[:alpha:]]+")

and you want a plain text caret (^) and not a Unicode circumflex accent
modifier (U+02c6).

* This problem is solved by removing the line exportPattern("<cb><86>[[:alpha:]]+").

## R CMD check results
* There were no ERRORs or WARNINGs. 
