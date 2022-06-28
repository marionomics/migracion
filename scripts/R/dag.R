
install.packages("dagitty")


g <- dagitty('dag {
  "Presence of Drug Cartels" [pos="-1.549,-1.001"]
  Poverty [pos="-0.400,-0.401"]
  Remittances [outcome,pos="0.804,-1.004"]
  "Presence of Drug Cartels" -> Remittances
  Poverty -> "Presence of Drug Cartels"
  Poverty -> Remittances
}')


