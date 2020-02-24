###########################################################
# AqMaD inlees functionaliteiten voor RShiny applicatie   #
#                                                         #
# Auteurs: Willem Stolte                                  #
#          Lilith Kramer                                  #
#          Marc Weeber                                    #
#                                                         #
# Datum : 2018-05-08                                      #
# Bedrijf: Deltares                                       #
# Licentie: GNU General Public License                    #
#                                                         #           
# Contact : Gerben van Geest                              #
# Email : gerben.vangeest@deltares.nl                     #
#                                                         #
########################################################### 

inlezen <- function(bestandsnaam){
  
  library(tidyverse)
  bestand <- read_delim(bestandsnaam, delim = ";")
  
}