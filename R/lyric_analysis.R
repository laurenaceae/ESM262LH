#' lyric_analysis
#'
#'This function takes in an artist name and returns the song names and sentiment values for that artist's top 100 hits.
#' @author Lauren Harris & Sarah Lam
#' @param artist The musical artist of interest (case insensitive)
#' @param dictionary The type of dictionary to use in the sentiment analysis (one of "GI" or "QDAP")
#' @param start_year The beginning of the date range to analyze, including this value (default is 0, but data starts at 1960)
#' @param end_year The end of the date range to analyze, including this value (default is the current year, but data goes up to 2016 currently)
#' @param exact True/False. Whether to look for an exact artist match or to use the artist as a prefix. (default is false. if exact, may not return songs that feature other artists)
#' @param duplicates True/False. Whether to keep duplicate entries of songs. Default is false. (specifically for songs that were hits for more than one year)
#' @return A data frame with names and sentiment values of top 100 songs from your specified artist

lyric_analysis <- function(artist, dictionary = "GI", duplicates = FALSE, start_year = 0, end_year = as.numeric(format(Sys.Date(), "%Y")), exact = FALSE){
  # subset data
  if(exact == FALSE){ # for the artist prefix, find entries within the date range (case insensitive for artist)
    my_lyrics <- lyrics[grepl(artist, ignore.case = TRUE, x = lyrics$artist) & lyrics$year >= start_year & lyrics$year <= end_year,]
    }
  if(exact == TRUE){ # for the exact artist match, find entries within the date range (case insensitive for artist)
    my_lyrics <- lyrics[grepl(paste0("\\<", artist,"\\>$"), ignore.case = TRUE, x = lyrics$artist) & lyrics$year >= start_year & lyrics$year <= end_year,]
    }
  if(exact != TRUE & exact != FALSE){ # if exact is anything but true or false, return error
    stop("Argument 'exact' must be TRUE or FALSE")
    }

    # analyze sentiment
  sentiment <- analyzeSentiment(my_lyrics[,4]) # analyze sentiment of the lyrics
  if(dictionary %in% c("GI", "QDAP") == FALSE) {
    stop("Dictionary not known") # if dictionary is not GI or QDAP, return error
    }
  my_dictionary <- paste0("Sentiment", dictionary) # use dictionary to call the correct column

    # make output data frame
  columns <- c("song_names", "value") # create an empty data frame to fill
  my_df <- data.frame(matrix(NA, nrow = nrow(my_lyrics), ncol = 2)) # this empty data frame is filled with NAs of a specific dimension
  colnames(my_df) = columns # rename columns to "song_names" and "value"
  my_df$song_names <- unlist(my_lyrics[,1]) # fill data frame with song names
  my_df$value <- unlist(sentiment[colnames(sentiment) == my_dictionary]) # fill data frame with sentiment values from specified dictionary

    # remove duplicates
  if(duplicates != TRUE & duplicates != FALSE){ # if duplicates is anything but true or false, return error
    stop("Argument 'duplicates' must be TRUE or FALSE")
    }
  if(duplicates == FALSE){ # if duplicates is set to false
    my_df <- distinct(my_df) # then remove double entries (songs of same name and value)
    }

    # send warnings
  if(is.na(sum(my_df$value)) == TRUE){ # return warning if any of the songs by selected artists do not have associated lyric data
    warning("One or more values return NA due to missing song lyrics")
    }

    # print output
  return(my_df) # return the data frame of song names and sentiment values
}

