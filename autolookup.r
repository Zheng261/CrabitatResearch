install.packages("curl")
library(curl)
install.packages("xml2")
library(xml2)

cleanFun <- function(htmlString) {
  return(gsub("&gt;",">",gsub("&lt;","<",gsub("<.*?>", "", htmlString))))
}

SOS <- function(thisQuery,nAnswers=5,r = TRUE) {
  if (r) {
    r <- GET("https://stackoverflow.com/search",query=list(q=paste(thisQuery,"in r")))
  } else {
    r <- GET("https://stackoverflow.com/search",query=list(q=thisQuery))
  }
  p <- content(r, "parse", type="text/html",encoding = "ISO-8859-1")
  answersToVisit = c();
  for (i in 1:nAnswers) {
    #Get next answer
    pos = regexpr('result-link', p)
    p <- substr(p,pos,10000000)
    #Parse out link 
    posStart = regexpr("href=\"",p)
    posEnd = regexpr("data-searchsession",p)
    pAnswersLink <- substr(p,posStart,posEnd)
    pAnswersLink <- substr(pAnswersLink,7,nchar(pAnswersLink)-3)
    if (nchar(pAnswersLink) < 1) {
       print("No more matching queries")
       break
    } else {
      x <- paste0("https://stackoverflow.com",pAnswersLink)
      answersToVisit = c(x,answersToVisit)
    }
    #Go over this answer
    p <- substr(p,posEnd,10000000)
  }
  x = 1
  
  if (length(answersToVisit) > 1) {
    iter = 1
    answersToVisit = c(answersToVisit,"Exit")
    for (answerSite in answersToVisit) {
      print(paste0("[",iter,"]",answerSite))
      iter = iter + 1
    }
    x <- as.numeric(readline(prompt=paste0("Which answer would you like to see? [1-",length(answersToVisit),"]")))
  }
  if (answersToVisit[x]=="Exit") {
    return("Exited")
  }
  answerPage <- GET(answersToVisit[x])
  answerPage <- content(answerPage, "parse", type="text/html",encoding = "ISO-8859-1")
  #grabs OP question
  firstQuestionIndex <- regexpr("postcell post-layout",answerPage)
  firstQuestionLastIndex <- regexpr("<div class=\"post-taglist\">",answerPage)
  substr(answerPage,firstQuestionIndex+33,firstQuestionLastIndex-1)
  trimmedQuestion <- substr(answerPage,firstQuestionIndex+33,firstQuestionLastIndex-1)
  print("#####################################")
  print("######### Original Question #########")
  print("#####################################")
  cat(cleanFun(trimmedQuestion))
  
  #grabs top Answer
  firstAnswerIndex <- regexpr("answers-header",answerPage)
  answerPage <- substr(answerPage,firstAnswerIndex,1000000)
  firstAnswerWordsIndex <- regexpr("answercell",answerPage)
  answerPage <- substr(answerPage,firstAnswerWordsIndex,1000000)
  firstAnswerInstructionsIndex <- regexpr("<p>",answerPage)
  firstAnswerLastInstructionsIndex <- regexpr("</div>",answerPage)
  trimmedAnswer <- substr(answerPage,firstAnswerInstructionsIndex,firstAnswerLastInstructionsIndex-1)
  print("#####################################")
  print("############ Top Answer #############")
  print("#####################################")
  cat(cleanFun(trimmedAnswer))
  cat(paste0(cleanFun(trimmedAnswer),"\n"))
}

############################
answerPage <- GET("https://stackoverflow.com/questions/11134812/how-to-find-the-length-of-a-string-in-r")
answerPage <- content(answerPage, "parse", type="text/html",encoding = "ISO-8859-1")
firstQuestionIndex <- regexpr("postcell post-layout",answerPage)
firstQuestionLastIndex <- regexpr("<div class=\"post-taglist\">",answerPage)
substr(answerPage,firstQuestionIndex+33,firstQuestionLastIndex-1)
trimmedQuestion <- substr(answerPage,firstQuestionIndex+33,firstQuestionLastIndex-1)
print("Original Question:")
cat(cleanFun(trimmedQuestion))
firstAnswerIndex <- regexpr("answers-header",answerPage)
answerPage <- substr(answerPage,firstAnswerIndex,1000000)
firstAnswerWordsIndex <- regexpr("answercell",answerPage)
answerPage <- substr(answerPage,firstAnswerWordsIndex,1000000)
firstAnswerInstructionsIndex <- regexpr("<p>",answerPage)
firstAnswerLastInstructionsIndex <- regexpr("</div>",answerPage)
trimmedAnswer <- substr(answerPage,firstAnswerInstructionsIndex,firstAnswerLastInstructionsIndex-1)
print("Top Answer:")
cat(cleanFun(trimmedAnswer))


#x <- as.numeric (readline(prompt="What is the value of x? "))
