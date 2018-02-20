#' Read characters from Nexus file
#'
#' Parses Nexus file, reading character states and names
#'
#' Tested with nexus files downloaded from MorphoBank with the "no notes"
#' option, but should also work more generally.
#'
#' Do [report](https://github.org/TGuillerme/Inapp/issues "New GitHub Issue")
#' incorrectly parsed files.
#'
#' @param filepath character string specifying location of file
#' @param character_num Index of character(s) to return.
#'                      `NULL`, the default, returns all characters.
#'
#' @return A matrix whose row names correspond to tip labels, and column names
#'         correspond to character labels, with the attribute `state.labels`
#'         listing the state labels for each character
#'
#' @author Martin R. Smith
#' @references
#'   Maddison, D. R., Swofford, D. L. and Maddison, W. P. (1997)
#'   NEXUS: an extensible file format for systematic information.
#'   Systematic Biology, 46, 590–621.
#' @export
#'
read.characters <- function (filepath, character_num=NULL) {
  lines <- readLines(filepath)
  nexusComment.pattern <- "\\[[^\\]*\\]"
  lines <- gsub(nexusComment.pattern, "", lines)
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  semicolons <- which(RightmostCharacter(lines) == ';')
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
    taxonLine.pattern <- "('([^']+)'|\"([^\"+])\"|(\\w+))\\s+(.*)$"

    taxa <- sub(taxonLine.pattern, "\\2\\3\\4", matrixLines, perl=TRUE)
    taxa <- gsub(" ", "_", taxa, fixed=TRUE)

    tokens <- sub(taxonLine.pattern, "\\5", matrixLines, perl=TRUE)
    tokens <- gsub("\t", "", gsub(" ", "", tokens, fixed=TRUE), fixed=TRUE)

    tokens.pattern <- "\\([^\\)]+\\)|\\{[^\\}]+\\}|."
    matches <- gregexpr(tokens.pattern, tokens, perl=TRUE)
    n_char <- length(matches[[1]])

    if (!exists("character_num") || is.null(character_num)) {
        character_num <- seq_len(n_char)
    } else if (any(character_num > n_char) || any(character_num < 1)) {
        warning ("Character number must be between 1 and ", n_char, ".")
        character_num[character_num < 1] <- 1
        character_num[character_num > n_char] <- n_char
    }

    tokens <- t(vapply(regmatches(tokens, matches),
                     function (x) x[character_num, drop=FALSE],
                     character(length(character_num))))
    if (length(character_num) == 1) {
      tokens <- t(tokens)
    } else if (length(character_num) == 0) {
      stop("No characters selected")
    }
    rownames(tokens) <- taxa

    labelStart <- which(upperLines == 'CHARLABELS')
    if (length(labelStart) == 1) {
      labelEnd <- semicolons[semicolons > labelStart][1]
      if (lines[labelEnd] == ';') labelEnd <- labelEnd - 1
      #attr(dat, 'char.labels')
      colnames(tokens) <- lines[labelStart + character_num]
    } else {
      if (length(labelStart) > 1)
        warning("Multiple CharLabels blocks in Nexus file.")
    }


    GetStates <- function (stateLines) {
        ## Written with MorphoBank format in mind: each label on separate line,
        ## each character introduced by integer and terminated with comma.
    }
    stateStart <- which(upperLines == 'STATELABELS')
    if (length(stateStart) == 1) {
        stateEnd <- semicolons[semicolons > stateStart][1]
        stateLines <- lines[stateStart:stateEnd]
        stateStarts <- grep("^\\d+", stateLines)
        stateEnds <- grep("[,;]$", stateLines)
        if (length(stateStarts) != length(stateEnds)) {
            warning("Could not parse character states.")
        } else {
            attr(tokens, 'state.labels') <-
            lapply(character_num, function (i)
                stateLines[(stateStarts[i] + 1):(stateEnds[i] - 1)]
            )
        }
    } else {
        if (length(labelStart) > 1)
            warning("Multiple StateLabels blocks in Nexus file.")
    }
  }

  # Return:
  tokens
}

#' @describeIn read.characters Read characters as phyDat object
#' @author Martin R. Smith
#' @importFrom phangorn phyDat
#' @export
read.as.phydat <- function (filepath) {
    tokens <- read.characters(filepath)
    allTokens <- unique(as.character(tokens))
    tokenNumbers <- seq_along(allTokens)
    names(tokenNumbers) <- allTokens
    matches <- gregexpr("[\\d\\-\\w]", allTokens, perl=TRUE)
    whichTokens <- regmatches(allTokens, matches)
    levels <- sort(unique(unlist(whichTokens)))
    whichTokens[allTokens == '?'] <- list(levels)
    contrast <- 1 * t(vapply(whichTokens, function (x) levels %in% x, logical(length(levels))))
    rownames(contrast) <- allTokens
    colnames(contrast) <- levels
    dat <- phangorn::phyDat(tokens, type='USER', contrast=contrast)
}

#' Rightmost character of string
#'
#' @author Martin R. Smith
#' @export
#' @keywords internal
RightmostCharacter <- function (string, len=nchar(string)) {
  substr(string, len, len)
}

