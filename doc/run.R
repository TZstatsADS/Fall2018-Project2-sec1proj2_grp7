library(shiny)
runApp(getwd())

library(rsconnect)
rsconnect::setAccountInfo(name='mc4398', 
                          token='59C79368DCAD21E928BB9EC5E1CB4B1D', 
                          secret='PlZLzSZee2x74CWlyeicfQDvHmFI7DLdMdozQQW6')
deployApp(account='mc4398')