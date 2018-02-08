#' Read characters from Nexus file
#'
#' Parses Nexus file, reading character states and names
#'
#' Tested with nexus files downloaded from MorphoBank with the "no notes"
#' option, but should also work more generally.
#'
#' Do (report)[https://github.org/TGuillerme/Inapp/issues] incorrectly parsed files.
#'
#' @param filepath character string specifying location of file
#' @param character.num Index of character(s) to return
#'
#' @return Character data
#'
#' @author Martin R. Smith
#' @export
#'
read.characters <- function (filepath, character.num=integer(0)) {
  lines <- readLines(filepath)
  nexusComment.pattern <- "\\[[^\\]*\\]"
  lines <- gsub(nexusComment.pattern, "", lines)
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  semicolons <- which(str.right(lines) == ';')
  upperLines <- toupper(lines)

  matrixStart <- which(upperLines == 'MATRIX')
  if (length(matrixStart) == 0) {
    warning("MATRIX block not found in Nexus file.")
  } else if (length (matrixStart) > 1) {
    warning("Multiple MATRIX blocks found in Nexus file.")
  } else {
    matrixEnd <- semicolons[semicolons > matrixStart][1]
    if (lines[matrixEnd] == ';') matrixEnd <- matrixEnd - 1

    matrixLines <- lines[(matrixStart + 1):matrixEnd]
    taxonLine.pattern <- "('([^']+)'|\"([^\"+])\"|(w+))\\s+(.*)$"

    matches <- regexpr(taxonLine.pattern, matrixLines, perl=TRUE)

    taxa <- sub(taxonLine.pattern, "\\2\\3\\4", matrixLines, perl=TRUE)
    taxa <- gsub(" ", "_", taxa, fixed=TRUE)

    tokens <- sub(taxonLine.pattern, "\\5", matrixLines, perl=TRUE)
    tokens <- sub("\t", "", sub(" ", "", tokens, fixed=TRUE), fixed=TRUE)

    tokens.pattern <- "\\([^\\)]+\\)|\\{[^\\}]+\\}|."
    matches <- gregexpr(tokens.pattern, tokens, perl=TRUE)
    tokens <- regmatches(tokens, matches)
    names(tokens) <- taxa

    allTokens <- unique(unlist(tokens))
    tokenNumbers <- seq_along(allTokens)
    names(tokenNumbers) <- allTokens


    matches <- gregexpr("[\\d\\-\\w]", allTokens, perl=TRUE)
    whichTokens <- regmatches(allTokens, matches)
    levels <- sort(unique(unlist(whichTokens)))
    whichTokens[allTokens == '?'] <- list(levels)
    contrast <- 1 * t(vapply(whichTokens, function (x) levels %in% x, logical(length(levels))))
    rownames(contrast) <- allTokens
    colnames(contrast) <- levels

    dat <- phyDat(tokens, type='USER', contrast=contrast)

    labelStart <- which(upperLines == 'CHARLABELS')
    if (length(labelStart) == 1) {
      labelEnd <- semicolons[semicolons > labelStart][1]
      if (lines[labelEnd] == ';') labelEnd <- labelEnd - 1
      attr(dat, 'char.labels') <- lines[(labelStart + 1):labelEnd]
    } else {
      if (length(labelStart) > 1)
        warning("Multiple CharLabels blocks in Nexus file.")
    }
  }

  # Return:
  dat
}

#' Rightmost character of string
#'
#' @author Martin R. Smith
#' @export
#' @keywords internal
str.right <- function (string, len=nchar(string)) {
  substr(string, len, len)
}

