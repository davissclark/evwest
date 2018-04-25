library(shiny)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(tidyr)
# library(showtext)

# font_add_google("Roboto", "roboto")
# showtext.auto()

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

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

# EIA: Average Residential Electricity Cost by State (July 2017)

getGearRatio <- function(car, gear = NA) {
  cars <- querydb("Cars")

  gears <- data.frame(Gear = c(1:6),
                      Ratio = as.numeric(cars[cars$Car15 == car,
                                              c('X1st',
                                                'X2nd',
                                                'X3rd',
                                                'X4th',
                                                'X5th',
                                                'Final.Drive')]
                      )
  )

  if (is.na(gear)) {
    gears
  } else {
    gears[gear, "Ratio"]
  }
}

getMotorLimit <- function(motor) {
  motor$Max.RPM
}

# getGearRatio("Acura  Integra RS")
# Gear Ratio
# 1    1  3.17
# 2    2  1.94
# 3    3  1.35
# 4    4  1.03
# 5    5  0.85
# 6    6  4.21
# getGearRatio("Acura  Integra RS", 1)
# 3.17

getDesignMass <- function() {
  components <- defaults[grepl("^mass_", names(defaults))]
  sum(as.numeric(unlist(components)), na.rm = TRUE)
}

getWheelRadius <- function(tire_width, tire_section_ht, tire_rim) {
  (((tire_width * 0.0393701 * (tire_section_ht/100) * 2) + tire_rim) / 2) / 12
}

getWheelCircumference <- function(wheel_radius) {
  wheel_radius * 2 * pi
}

getRotationalMass <- function(gear) {
  recode(gear,
         `1` = 1.19,
         `2` = 1.12,
         `3` = 1.08,
         `4` = 1.07,
         `5`	= 1.06)
}

getRollingResistCoef <- function(psi) {
  switch(as.character(psi),
         `30` = list(x2 = 1.23024511362609E-06,
                     x = 2.00E-18,
                     c = 0.0100),
         `45` = list(x2 = 8.24E-07,
                     x = -9.00E-19,
                     c = 0.0083),
         `60` = list(x2 = 6.00E-07,
                     x = 2.00E-18,
                     c = 0.0075)
  )
}

getVelocity <- function(velocity, acceleration, time_step) {
  velocity + (acceleration * time_step)
}

getSpeed <- function(velocity) {
  mph_fps(velocity)
}
# getSpeed(1.6)
# 1.090907

getMotorRpm <- function(velocity,
                        gear_ratio,
                        final_drive,
                        wheel_circumference) {
  (velocity * gear_ratio * final_drive * 60) / wheel_circumference
}

motorRpm <- function(motor_rpm, motor_limit) {
  if (motor_rpm > motor_limit) {
    motor_limit / 1000
  } else {
    motor_rpm / 1000
  }
}

getMotorTorque <- function(motor, count, motor_rpm = NA) {
  if (is.na(motor_rpm)) {
    motor$T0 * count
  } else {
    (motor_rpm ^ 4 * (motor$T4 * count)) +
      (motor_rpm ^ 3 * (motor$T3 * count)) +
      (motor_rpm ^ 2 * (motor$T2 * count)) +
      (motor_rpm ^ 1 * (motor$T1 * count)) + motor$T0 * count
  }
}

# getMotorTorque(motors[1,], 5)
# [1] 59.1255
# getMotorTorque(motors[1,])
# [1] 105.38

getWheelTorque <- function(motor_torque, gear_ratio, final_drive) {
  motor_torque * gear_ratio * final_drive
}

getWheelForce_gross <- function(wheel_torque, wheel_radius) {
  wheel_torque / wheel_radius
}

getDTForce <- function(wheel_force, drive_efficiency) {
  wheel_force * (1 - drive_efficiency)
}

getAeroForce <- function(frontal_area, Cd, velocity, rho) {
  0.5 * rho * frontal_area * Cd * velocity ^ 2
}

getRollingForce <- function(psi, speed, design_mass) {
  coef <- getRollingResistCoef(as.character(psi))
  ((coef$x2 * speed ^ 2) +
      (coef$x * speed) +
      coef$c) * design_mass
}

getWheelForce_net <- function(wheel_force, dt_force, aero_force, rolling_force) {
  wheel_force - dt_force - aero_force - rolling_force
}

getAcceleration <- function(wheelforce, design_mass, gear) {
  rotational_mass <- getRotationalMass(as.character(gear))
  wheelforce / lbf_slugs((design_mass * rotational_mass))
}

getDistance <- function(velocity0, velocity, time_step, dist0 = NA) {
  if (is.na(dist0)) {
    0
  }
  dist0 + ((velocity0 + velocity) / 2) * time_step
}


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