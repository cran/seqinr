#
# To translate sequences:
#

getTrans <- function(object, sens = "F", NAstring = "X", ambiguous = FALSE, force.first.aa.to.Met = FALSE, ...)
    UseMethod("getTrans")

getTrans.default <- function(object, sens = "F", NAstring = "X", ambiguous = FALSE, force.first.aa.to.Met = FALSE, ...)
    stop(paste("no getTrans method for objects of class:", class(object)))

getTrans.list <- function(object, sens = "F", NAstring = "X", ambiguous = FALSE, force.first.aa.to.Met = FALSE, ...)
    lapply(seq_len(length(object)),
           function(i) getTrans(object[[i]], sens = sens, NAstring = NAstring, ambiguous = ambiguous,
                                force.first.aa.to.Met = force.first.aa.to.Met, ...))

getTrans.character <- function(object, sens = "F", NAstring = "X", ambiguous = FALSE, force.first.aa.to.Met = FALSE,
                               ..., frame = 0, numcode = 1)
    translate(seq = object, frame = frame, sens = sens, numcode = numcode, NAstring = NAstring, ambiguous = ambiguous,
              force.first.aa.to.Met = force.first.aa.to.Met)

getTrans.SeqFastadna <- function(object, sens = "F", NAstring = "X", ambiguous = FALSE, force.first.aa.to.Met = FALSE,
                                 ..., frame = 0, numcode = 1){
    dnaseq <- getSequence(object, as.string = FALSE)
    translate(seq = dnaseq, frame = frame, sens = sens, numcode = numcode, NAstring = NAstring, ambiguous = ambiguous, 
              force.first.aa.to.Met = force.first.aa.to.Met)
}
getTrans.SeqFrag <- getTrans.SeqFastadna

getTrans.SeqAcnucWeb <- function(object, sens = "F", NAstring = "X", ambiguous = FALSE, force.first.aa.to.Met = FALSE,
                                 ..., frame = "auto", numcode = "auto"){
    dnaseq <- getSequence(object, as.string = FALSE)
    if(numcode == "auto") numcode <- attr(object, "ncbigc")
    if(frame == "auto") frame <- attr(object, "frame")
    translate(seq = dnaseq, frame = frame, sens = sens, numcode = numcode, NAstring = NAstring, ambiguous = ambiguous, 
              force.first.aa.to.Met = force.first.aa.to.Met) 
}

getTrans.qaw <- function(object, sens = "F", NAstring = "X", ambiguous = FALSE, force.first.aa.to.Met = FALSE, ...) 
                getTrans(object$req, force.first.aa.to.Met = force.first.aa.to.Met, ...)

getTrans.logical <- function (object, sens = "F", NAstring = "X", ambiguous = FALSE, force.first.aa.to.Met = FALSE, ...)
    object # so that NA is returned for virtual lists

