## Got these functions from http://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences

convert_text_to_sentences <- function(text, lang = "en") {
    # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
    sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
    
    # Convert text to class String from package NLP
    text <- as.String(text)
    
    # Sentence boundaries in text
    sentence.boundaries <- annotate(text, sentence_token_annotator)
    
    # Extract sentences
    sentences <- text[sentence.boundaries]
    
    # return sentences
    return(sentences)
}

reshape_corpus <- function(current.corpus, FUN, ...) {
    # Extract the text from each document in the corpus and put into a list
    text <- lapply(current.corpus, Content)
    
    # Basically convert the text
    docs <- lapply(text, FUN, ...)
    docs <- as.vector(unlist(docs))
    
    # Create a new corpus structure and return it
    new.corpus <- Corpus(VectorSource(docs))
    return(new.corpus)
}