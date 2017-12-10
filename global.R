library(shiny)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(showtext)

font_add_google("Roboto", "roboto")
showtext.auto()

querydb <- function(table = NA, q = NA) {
  db <- dbConnect(SQLite(), "data/db")

  if (is.na(q)) {
    data <- dbReadTable(db, table)

  } else {
    data <- dbGetQuery(db, q)
  }
  return(data)
  dbDisconnect(db)
}

writedb <- function(data, table) {
  db <- dbConnect(SQLite(), "data/db")
  dbWriteTable(db, table, data, overwrite = TRUE)
  dbDisconnect(db)
}

tablelistdb <- function() {
  db <- dbConnect(SQLite(), "data/db")
  return(dbListTables(db))
  dbDisconnect(db)
}

carSelect <- querydb("Cars")$Car15
motorSelect <- querydb("Motors")$Motor


#########
# Energy table from EIA: Average Residential Electricity Cost by State (July 2017)


######### unit conversions #############

mm_inch <- function(mm) {
  mm * 0.0393701
}

ftlbf_kwh <- function(ftlbf) {
  ftlbf * 0.0000003765843619189
}

mph_fps <- function(mph) {
  mph / 1.46667
}

mph_fps2 <- function(mph) {
  mph * 1.46667
}

torquerpm_hp <- function(torque) {
  torque * (1/5252)
}

lbf_slugs <- function(lbf) {
  lbf * 0.031081
}

hp_ftlbfpsec <- function(hp) {
  hp * 550
}

ft_miles <- function(ft) {
  ft / 5280
}

# model.setup <- list(`Time Step (s)` =	 0.20 ,
#                     `Shift Separation17` =	 0.100 ,
#                     `Motor Efficiency` =	.85,
#                     `Regeneration` =	.85,
#                     `Charge Efficency` =	.85,
#                     `Rolling Resistance Coefficient` =	 0.02 ,
#                     `Engine to Car Weight` =	.15,
#                     `Passenger Weight (lbm)` =	 300 ,
#                     `Cargo Weight (lbm)` =	 50 ,
#                     `Air Density (slugs/ft3)` =	 0.0023769 ,
#                     `Battery Density (lbm/kWh)` =	 20.0 ,
#                     `Default Tire Pressure (psi)` =	 30 ,
#                     `Averge Car Weight18` =	 2500 ,
#                     `Nominal Fixed DT Losses (hp)` =	 10 ,
#                     `Battery Cost ($/kWh)` =	 230 ,
#                     `Other Costs` = 	.3)

drive.type.dt.losses <- list(`Manual`	= 0,
                             `Modern Automatic` = .02,
                             `Conventional Automatic` = .05)


# gear0 <- 1
# gearratio0 <- getGearRatio(car, gear0)
# velocity0 <- 0
# speed0 <- getSpeed(velocity0)
# motortorque0 <- getMotorTorque(motor)
# wheeltorque0 <- getWheelTorque(motortorque0, gearratio0, getGearRatio(car, 6))
# wheelforce_gross0 <- getWheelForce_gross(wheeltorque0, wheelradius)
# dtforce0 <- getDTForce(wheelforce_gross0, drive.efficiency)
# aeroforce0 <- getAeroForce(aero_frontal_area, aero_drag_coeff, velocity0, rho)
# rollingforce0 <- getRollingForce(aero_frontal_area, aero_drag_coeff, velocity0, speed0, design.mass)
# wheelforce_net0 <- getWheelForce_net(wheelforce_gross0, dtforce0, aeroforce0, rollingforce0)
# acceleration0 <- getAcceleration(wheelforce_net0, design.mass, gear0)