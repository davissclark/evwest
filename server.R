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
  switch(gear,
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

shinyServer(function(input, output, session) {

  defaults <- reactiveValues(
    aero_drag_coeff = 0,
    aero_frontal_area = 0,
    aero_drive_type = "RWD",
    no_gears = 0,
    tire_width = 0,
    tire_section_height = 0,
    tire_rim = 0
  )

  setup <- reactiveValues(
    time_step = .2,
    shift_separation = .1,
    motor_efficiency = .85,
    regeneration = .85,
    charge_efficiency = .85,
    rolling_resistance_coef = .02,
    engine_carwt = .15,
    passenger_wt = 300,
    cargo_wt = 50,
    air_density = 0.0023769,
    rho = 0.0023769,
    battery_density = 20,
    tire_pressure = 30,
    avg_car_wt = 2500,
    nominal_fxd_dt_loss = 10,
    battery_cost = 230,
    other_costs = .3
  )

  mass <- reactiveValues(
    car = 0,
    battery = 0,
    motor = 0,
    passengers = 0,
    cargo = 0,
    engine_wt_removed = 0
  )

  config <- reactiveValues(
    car = "Generic Aero Small Car-RWD",
    motor = "34-96"
  )

  observeEvent(input$car, {
    config$car <- input$car
  })

  observeEvent(input$motor, {
    config$motor <- input$motor
  })

  output$views <- renderUI({
    switch(input$viewbar,
           config = tagList(
             fluidRow(
               column(3,
                      # br(),
                      div(class = "tools",
                          style = "height: 40px;
                          font-size: 14px;
                          font-weight: 500;
                          margin-top: 10px;",
                        "Inputs"
                      ),
                      div(
                        style = "
                        margin: 0 0 5px;",
                        div(
                          class = "tools",
                          selectInput("state",
                                      "State",
                                      choices = state.name,
                                      selected = "California"),

                          numericInput("annual_miles",
                                       label = "Annual Driving Miles",
                                       12000,
                                       0,
                                       100000,
                                       1000),

                          numericInput("avgmpg",
                                       label = "Average MPG",
                                       30,
                                       5,
                                       1000,
                                       1)
                        ),
                        div(class = "tools",
                            selectInput("car",
                                        "Donor Car",
                                        choices = c("", carSelect),
                                        selected = ifelse(!is.null(config$car), config$car, ""))
                        ),
                        div(
                          class = "tools",
                          fluidRow(
                            column(8,
                                   selectInput("motor",
                                               label = "Motor Type",
                                               choices = c("", motorSelect),
                                               selected = ifelse(!is.null(config$motor), config$motor, ""))
                            ),
                            column(4,
                                   numericInput("no.motors",
                                                "#", 1,
                                                1, 3,
                                                step = 1)
                            )
                          )
                        ),
                        div(
                          class = "tools",
                          numericInput("battery.size",
                                       label = "Battery Size (kWh)",
                                       30,
                                       0,
                                       1000,
                                       1)
                        ),
                        actionButton("clear",
                                     label = "Clear",
                                     width = "100%"),
                        actionButton("run",
                                     label = "Calculate",
                                     width = "100%"),
                        br()
                      )
             ),
             column(9,
                    fluidRow(
                      column(6,
                             class = 'details',
                             DT::dataTableOutput("cardetails"),
                             DT::dataTableOutput("geardetails"),
                             DT::dataTableOutput("tiredetails"),
                             DT::dataTableOutput("motordetails"),
                             br()
                      ),
                      column(6,
                             DT::dataTableOutput("performance.summary"),
                             br(),
                             # br(),
                             br(),
                             plotOutput("performancechart", height = "100%"),
                             br()
                      )
                    )
             ))
  ),
  performance = tagList(
    fluidRow(
      column(9, offset = 3,
             DT::dataTableOutput("performancetable"))
    )
  ),
  range = tagList(
    fluidRow(
      column(9, offset = 3,
             DT::dataTableOutput("range"))
    )
  ),
  database = tagList(
    fluidRow(
      column(3,
             selectInput("dataset",
                         "Select table",
                         tablelistdb(),
                         selected = "UpdateLog"),
             conditionalPanel(
               condition = 'input.dbTable_rows_selected.length > 0',
               actionButton("edit", "Edit", width = '100%')
             )
      ),
      column(9,
             DT::dataTableOutput("dbTable")
      )
    ))
    )
  })

  motor <- reactive({
    req(input$motor)
    querydb("Motors") %>%
      filter(Motor == config$motor)
  })

  car <- reactive({
    req(input$car)
    querydb("Cars") %>%
      filter(Car15 == config$car)
  })

  observe({
    defaults$no_gears <- car()$X..Gears
  })

  observe({
    defaults$aero_drive_type <- car()$Drive
  })

  observe({
    defaults$aero_frontal_area <- car()$Ft2.20
  })

  observe({
    defaults$aero_drag_coeff <- car()$Cd
  })

  observe({
    defaults$tire_width <- car()$Width.Side.Wall..mm.
  })

  observe({
    defaults$tire_section_height <- car()$Aspect.Side.Wall..mm.
  })

  observe({
    defaults$tire_rim <- car()$Rim.Radius..in.
  })

  observe({
    mass$car <- car()$Lbsm
  })

  observe({
    mass$motor <- motor()$Mass..lbm. * input$no.motors
  })

  observe({
    mass$passengers <- setup$passenger_wt
  })

  observe({
    mass$cargo <- setup$cargo_wt
  })

  observe({
    mass$battery <- input$battery.size * setup$battery_density
  })

  observe({
    mass$engine_wt_removed <- setup$engine_carwt * mass$car
  })

  dt.loss <- reactive({
    switch(defaults$aero_drive_type,
           `FWD` = .10,
           `RWD` = .12,
           `RE-RWD` = .12,
           `AWD` = .14,
           `4WD` = .16)
  })

  gear.ratios <- reactive({
    getGearRatio(input$car)
  })

  torque <- reactive({
    motor()$Torque..ft.lbf.25 * input$no.motors
  })

  power <- reactive({
    motor()$Power..hp.25 * input$no.motors
  })

  design.mass <- reactive({

    req(mass)
    nums <- as.numeric(unlist(reactiveValuesToList(mass)))
    nums <- sum(nums[-length(nums)], na.rm = TRUE) - nums[length(nums)]

    if (is.na(nums)) {
      0
    } else {
      nums
    }

  })

  # max.power <- reactive({
  #   motor()$Power..hp.25 * input$no.motors
  # })

  max.dt.loss <- reactive({
    fixed.dt.loss <- setup$nominal_fxd_dt_loss * (car()$Lbsm / setup$avg_car_wt)
    var.dt.loss <- dt.loss()
    (power() * var.dt.loss) + fixed.dt.loss
  })

  drive.efficiency <- reactive({
    (power() - max.dt.loss()) / power()
  })

  wheelRadius <- reactive({
    getWheelRadius(defaults$tire_width,
                   defaults$tire_section_height,
                   defaults$tire_rim)
  })

  wheelCircumference <- reactive({
    getWheelCircumference(wheelRadius())
  })

  performance <- reactiveValues(
    results = NULL
  )

  max.acceleration <- reactive({
    req(performance$results)

    max(performance$results$acceleration, na.rm = TRUE)
  })

  max.g.force <- reactive({
    req(performance$results)

    lbf_slugs(max.acceleration())
  })

  zero50 <- reactive({
    req(performance$results)

    r <- performance$results %>%
      filter(speed < 50)

    max(r$time, na.rm = TRUE)
  })

  zero70 <- reactive({
    req(performance$results)

    r <- performance$results %>%
      filter(speed < 70)

    max(r$time, na.rm = TRUE)
  })

  min.acc <- reactive({
    0.01
  })

  observeEvent(input$run, {

    withProgress(message = 'Calculating Performance Diagnostics', value = 0, {

    time_step <- 0
    gear0 <- 1
    gearratio0 <- getGearRatio(input$car, gear0)
    velocity00 <- 0
    speed0 <- getSpeed(velocity00)
    motortorque0 <- getMotorTorque(motor(), input$no.motors)
    wheeltorque0 <- getWheelTorque(motortorque0, gearratio0, getGearRatio(input$car, 6))
    wheelforce_gross0 <- getWheelForce_gross(wheeltorque0, wheelRadius())
    dtforce0 <- getDTForce(wheelforce_gross0, drive.efficiency())
    aeroforce0 <- getAeroForce(defaults$aero_frontal_area, defaults$aero_drag_coeff, velocity00, setup$rho)
    rollingforce0 <- getRollingForce(setup$tire_pressure, speed0, design.mass())
    wheelforce_net0 <- getWheelForce_net(wheelforce_gross0, dtforce0, aeroforce0, rollingforce0)
    acceleration0 <- getAcceleration(wheelforce_net0, design.mass(), 1)

    round <- data.frame(time = time_step,
                        gear = gear0,
                        gearratio = gearratio0,
                        velocity0 = velocity00,
                        velocity = velocity00,
                        speed = speed0,
                        motorrpm = 0,
                        motorrpmpt = 0,
                        motortorque = motortorque0,
                        wheeltorque = wheeltorque0,
                        wheelforce.gross = wheelforce_gross0,
                        dtforce = dtforce0,
                        aeroforce = aeroforce0,
                        rollingforce = rollingforce0,
                        wheelforce.net = wheelforce_net0,
                        acceleration = acceleration0,
                        distance_ft = 0,
                        distance_miles = 0,
                        hp = 0)

    gear0 <- 2
    gearratio0 <- getGearRatio(input$car, gear0)
    velocity00 <- 0
    speed0 <- getSpeed(velocity00)
    motortorque0 <- getMotorTorque(motor(), input$no.motors)
    wheeltorque0 <- getWheelTorque(motortorque0, gearratio0, getGearRatio(input$car, 6))
    wheelforce_gross0 <- getWheelForce_gross(wheeltorque0, wheelRadius())
    dtforce0 <- getDTForce(wheelforce_gross0, drive.efficiency())
    aeroforce0 <- getAeroForce(defaults$aero_frontal_area, defaults$aero_drag_coeff, velocity00, setup$rho)
    rollingforce0 <- getRollingForce(setup$tire_pressure, speed0, design.mass())
    wheelforce_net0 <- getWheelForce_net(wheelforce_gross0, dtforce0, aeroforce0, rollingforce0)
    acceleration0 <- getAcceleration(wheelforce_net0, design.mass(), gear0)

    round_alt <- data.frame(time = time_step,
                            gear = gear0,
                            gearratio = gearratio0,
                            velocity0 = velocity00,
                            velocity = velocity00,
                            speed = speed0,
                            motorrpm = 0,
                            motorrpmpt = 0,
                            motortorque = motortorque0,
                            wheeltorque = wheeltorque0,
                            wheelforce.gross = wheelforce_gross0,
                            dtforce = dtforce0,
                            aeroforce = aeroforce0,
                            rollingforce = rollingforce0,
                            wheelforce.net = wheelforce_net0,
                            acceleration = acceleration0,
                            distance_ft = 0,
                            distance_miles = 0,
                            hp = 0)


    incProgress(1)

    i <- 1

    while (round[i, "acceleration"] > min.acc()) {
      zero <- i
      i <- i + 1

      if (round[zero, "acceleration"] < round_alt[zero, "acceleration"] & round[zero, "gear"] < defaults$no_gears) {
        gear <- round[zero, "gear"] + 1
      } else {
        gear <- round[zero, "gear"]
      }

      step <- setup$time_step * gear
      time <- round[zero, "time"] + step

      round[i, "time"] <- time

      round[i, "gear"] <- gear

      round[i, "gearratio"] <- getGearRatio(input$car, gear)

      round[i, "velocity0"] <- round[zero, "velocity"]

      round[i, "velocity"] <- getVelocity(round[i, "velocity0"],
                                          round[zero, "acceleration"],
                                          step)

      round[i, "speed"] <- getSpeed(round[i, "velocity"])

      round[i, "motorrpm"] <- getMotorRpm(round[i, "velocity"],
                                          getGearRatio(input$car, gear),
                                          getGearRatio(input$car, 6),
                                          wheelCircumference())

      round[i, "motorrpmpt"] <- ifelse(round[i, "motorrpm"] > getMotorLimit(motor()),
                                       getMotorLimit(motor()) / 1000,
                                       round[i, "motorrpm"] / 1000)

      round[i, "motortorque"] <- getMotorTorque(motor(),
                                                input$no.motors,
                                                round[i, "motorrpmpt"])

      round[i, "wheeltorque"] <- getWheelTorque(round[i, "motortorque"],
                                                round[i, "gearratio"],
                                                getGearRatio(input$car, 6))

      round[i, "wheelforce.gross"] <- getWheelForce_gross(round[i, "wheeltorque"],
                                                          wheelRadius())

      round[i, "dtforce"] <- getDTForce(round[i, "wheelforce.gross"],
                                        drive.efficiency())

      round[i, "aeroforce"] <- getAeroForce(defaults$aero_frontal_area,
                                            defaults$aero_drag_coeff,
                                            round[i, "velocity"],
                                            setup$rho)

      round[i, "rollingforce"] <- getRollingForce(setup$tire_pressure,
                                                  round[i, "speed"],
                                                  design.mass())

      round[i, "wheelforce.net"] <- getWheelForce_net(round[i, "wheelforce.gross"],
                                                      round[i, "dtforce"],
                                                      round[i, "aeroforce"],
                                                      round[i, "rollingforce"])

      round[i, "acceleration"] <- getAcceleration(round[i, "wheelforce.net"],
                                                  design.mass(),
                                                  round[i, "gear"])

      round[i, "distance_ft"] <- getDistance(round[i, "velocity0"],
                                             round[i, "velocity"],
                                             step,
                                             round[zero, "distance_ft"])

      round[i, "distance_miles"] <- ft_miles(round[i, "distance_ft"])

      round[i, "hp"] <- torquerpm_hp(round[i, "motortorque"] * round[i, "motorrpm"])

      gear_alt <- ifelse(gear >= defaults$no_gears, 5, gear + 1)
      round_alt[i, "time"] <- time
      round_alt[i, "gear"] <- gear_alt
      round_alt[i, "gearratio"] <- getGearRatio(input$car, gear_alt)
      round_alt[i, "velocity0"] <- round_alt[zero, "velocity"]
      round_alt[i, "velocity"] <- getVelocity(round_alt[i, "velocity0"],
                                              round_alt[zero, "acceleration"],
                                              step)
      round_alt[i, "speed"] <- getSpeed(round_alt[i, "velocity"])
      round_alt[i, "motorrpm"] <- getMotorRpm(round_alt[i, "velocity"],
                                              getGearRatio(input$car, gear_alt),
                                              getGearRatio(input$car, 6),
                                              wheelCircumference())
      round_alt[i, "motorrpmpt"] <- ifelse(round_alt[i, "motorrpm"] > getMotorLimit(motor()), getMotorLimit(motor()) / 1000, round_alt[i, "motorrpm"] / 1000)
      round_alt[i, "motortorque"] <- getMotorTorque(motor(), input$no.motors, round_alt[i, "motorrpmpt"])
      round_alt[i, "wheeltorque"] <- getWheelTorque(round_alt[i, "motortorque"], round_alt[i, "gearratio"], getGearRatio(input$car, 6))
      round_alt[i, "wheelforce.gross"] <- getWheelForce_gross(round_alt[i, "wheeltorque"], wheelRadius())
      round_alt[i, "dtforce"] <- getDTForce(round_alt[i, "wheelforce.gross"], drive.efficiency())
      round_alt[i, "aeroforce"] <- getAeroForce(defaults$aero_frontal_area, defaults$aero_drag_coeff, round_alt[i, "velocity"], setup$rho)
      round_alt[i, "rollingforce"] <- getRollingForce(setup$tire_pressure, round_alt[i, "speed"], design.mass())
      round_alt[i, "wheelforce.net"] <- getWheelForce_net(round_alt[i, "wheelforce.gross"], round_alt[i, "dtforce"], round_alt[i, "aeroforce"], round_alt[i, "rollingforce"])
      round_alt[i, "acceleration"] <- getAcceleration(round_alt[i, "wheelforce.net"], design.mass(), gear_alt)
      round_alt[i, "distance_ft"] <- getDistance(round_alt[i, "velocity0"],
                                                 round_alt[i, "velocity"],
                                                 step,
                                                 round_alt[zero, "distance_ft"])
      round_alt[i, "distance_miles"] <- ft_miles(round_alt[i, "distance_ft"])
      round_alt[i, "hp"] <- torquerpm_hp(round_alt[i, "motortorque"] * round_alt[i, "motorrpm"])
    }
    })
    performance$results <- select(round, -velocity0)
  })


  range <- reactiveValues(
    simulation = querydb("Range"),
    results = NULL
  )

  observeEvent(performance$results, {

    if (is.null(performance$results))
      return(NULL)

    withProgress(message = 'Running Range Simulation', value = 0, {
      incProgress(1/nrow(range$simulation))

      r <- matrix(nrow = nrow(range$simulation), ncol = 13) %>% as.data.frame()
      names(r) <- c(names(range$simulation), "velocity", "distance", "acceleration", "gear", "mass", "inertial", "aero", "rolling", "dt", "motor")

      r[1, 1:3] <- range$simulation[1,]
      r[1, "velocity"] <- mph_fps(r[1, "MPH"])
      r[1, "distance"] <- 0
      r[1, "acceleration"] <- 0
      r[1, "gear"] <- filter(performance$results, velocity <= r[1, "velocity"]) %>%
        .$gear %>%
        max(na.rm = TRUE)
      r[1, "mass"] <- getRotationalMass(r[1, "gear"]) * design.mass()

      r[1, "inertial"] <- lbf_slugs(r[1, "mass"]) * r[1, "acceleration"]
      r[1, "aero"] <- getAeroForce(defaults$aero_frontal_area, defaults$aero_drag_coeff, r[1, "velocity"], setup$rho)
      r[1, "rolling"] <- ifelse(r[1, "velocity"] > 0, getRollingForce(setup$tire_pressure, r[1, "MPH"], design.mass()), 0)
      r[1, "dt"] <- abs(sum(c(r[1, "inertial"], r[1, "aero"], r[1, "rolling"]))) * (1 - drive.efficiency())
      r[1, "motor"] <- ifelse(sum(c(r[1, "inertial"], r[1, "aero"], r[1, "rolling"], r[1, "dt"])) >= 0,
                              sum(c(r[1, "inertial"], r[1, "aero"], r[1, "rolling"], r[1, "dt"])) / setup$motor_efficiency,
                              sum(c(r[1, "inertial"], r[1, "aero"], r[1, "rolling"], r[1, "dt"])) * setup$regeneration)


      for (i in 2:nrow(range$simulation)) {
        incProgress(1/nrow(range$simulation))
        r[i, 1:3] <- range$simulation[i,]
        r[i, "velocity"] <- mph_fps2(r[i, "MPH"])
        r[i, "distance"] <- ((r[i, "velocity"] + r[i-1, "velocity"])/2/5280) + r[(i-1), "distance"]
        r[i, "acceleration"] <- (r[i, "velocity"] - r[i-1, "velocity"])
        r[i, "gear"] <- filter(performance$results, velocity <= r[i, "velocity"]) %>%
          .$gear %>%
          max(na.rm = TRUE)
        r[i, "mass"] <- getRotationalMass(r[i, "gear"]) * design.mass()

        r[i, "inertial"] <- lbf_slugs(r[i, "mass"]) * r[i, "acceleration"]
        r[i, "aero"] <- getAeroForce(defaults$aero_frontal_area, defaults$aero_drag_coeff, r[i, "velocity"], setup$rho)
        r[i, "rolling"] <- ifelse(r[i, "velocity"] > 0, getRollingForce(setup$tire_pressure, r[i, "MPH"], design.mass()), 0)
        r[i, "dt"] <- abs(sum(c(r[i, "inertial"], r[i, "aero"], r[i, "rolling"]))) * (1 - drive.efficiency())
        r[i, "motor"] <- ifelse(sum(c(r[i, "inertial"], r[i, "aero"], r[i, "rolling"], r[i, "dt"])) >= 0,
                                sum(c(r[i, "inertial"], r[i, "aero"], r[i, "rolling"], r[i, "dt"])) / setup$motor_efficiency,
                                sum(c(r[i, "inertial"], r[i, "aero"], r[i, "rolling"], r[i, "dt"])) * setup$regeneration)
      }

      r <- r %>%
        mutate(inertial = inertial * velocity,
               aero = aero * velocity,
               rolling = rolling * velocity,
               dt = dt * velocity,
               motor = motor * velocity)
    })
    range$results <- r
  })

  output$maxacc <- renderText({
    round(max.acceleration(), 2)
  })

  output$maxgforce <- renderText({
    round(max.g.force(), 2)
  })

  output$zero60 <- renderText({
    zero60()
  })

  output$cardetails <- DT::renderDataTable({
    req(input$car)
    d <- car() %>%
      mutate(`Design Mass (lbm)` = round(design.mass(), 0)) %>%
      # mutate(X1st = formatC(X1st, digits = 2, format = 'f'),
      #        X2nd = formatC(X2nd, digits = 2, format = 'f'),
      #        X3rd = formatC(X3rd, digits = 2, format = 'f'),
      #        X4th = formatC(X4th, digits = 2, format = 'f'),
      #        X5th = formatC(X5th, digits = 2, format = 'f'),
      #        Final.Drive = formatC(Final.Drive, digits = 2, format = 'f')) %>%
      select(Car15,
             `Drag Coefficient (Cd)` = Cd,
             `Frontal Area (sqft)` = `Ft2.20`,
             `Design Mass (lbm)`) %>%
             # `1` = X1st,
             # `2` = X2nd,
             # `3` = X3rd,
             # `4` = X4th,
             # `5` = X5th,
             # `Final Drive` = Final.Drive) %>%
      gather(Car, Value, Car15:`Design Mass (lbm)`) %>%
      mutate(Value = ifelse(Car == "Frontal Area (sqft)",
                            formatC(as.numeric(Value), digits = 1, format = 'f'),
                            formatC(as.numeric(Value), digits = 2, format = 'f'))) %>%
      filter(Car != "Car15")

    d[3, 2] <- prettyNum(round(as.numeric(d[3, 2]), 0), big.mark = ',')

    d %>%
      datatable(
        colnames = c(input$car, ""),
        selection = 'none',
        options = list(
          columnDefs = list(list(className = 'dt-right', targets = 1)),
          searching = FALSE,
          paging = FALSE,
          dom = 'ft',
          ordering = FALSE
        ),
        rownames = FALSE) %>%
      formatStyle(1:2,
                  backgroundColor = 'white')
  })

  output$geardetails <- DT::renderDataTable({
    req(input$car)
    d <- car() %>%
      select(`1st` = X1st,
             `2nd` = X2nd,
             `3rd` = X3rd,
             `4th` = X4th,
             `5th` = X5th,
             `Final Drive` = Final.Drive) %>%
      gather(Car, Value, `1st`:`Final Drive`) %>%
      mutate(Value = formatC(as.numeric(Value), digits = 2, format = 'f'))
      # filter(Car != "Car15")

    d %>%
      datatable(
        colnames = c("Gear Ratios", ""),
        selection = 'none',
        options = list(
          columnDefs = list(list(className = 'dt-right', targets = 1)),
          searching = FALSE,
          paging = FALSE,
          dom = 'ft',
          ordering = FALSE
        ),
        rownames = FALSE) %>%
      formatStyle(1:2,
                  backgroundColor = 'white')
  })

  output$tiredetails <- DT::renderDataTable({

    d <- data.frame(width = defaults$tire_width,
               sheight = defaults$tire_section_height,
               rim = defaults$tire_rim,
               radius = round((wheelRadius() * 2) * 12, 2),
               stringsAsFactors = FALSE) %>%
      gather(Key, Value, width:radius)

    d[, 1] <- c("Width (cm)",
                "Section Height (%)",
                "Rim (in)",
                "Diameter (in)")

    d %>%
      datatable(colnames = c("Tire Size", ""),
                selection = 'none',
                options = list(
                  searching = FALSE,
                  paging = FALSE,
                  dom = 'ft',
                  ordering = FALSE
                ),
                rownames = FALSE) %>%
      formatStyle(1:2,
                  backgroundColor = 'white')
  })

  output$motordetails <- DT::renderDataTable({
    motor() %>%
      select(Motor,
             `Torque (ft-lbs)` = Torque..ft.lbf.25,
             `Power (hp)` = Power..hp.25) %>%
      gather(Motor, Value, Motor:`Power (hp)`) %>%
      mutate(Value = as.numeric(Value) * input$no.motors) %>%
      mutate(Value = formatC(as.numeric(Value), digits = 1, format = 'f')) %>%
      filter(Motor != "Motor") %>%
      datatable(
        colnames = c(input$motor, sprintf("x%s", input$no.motors)),
        selection = 'none',
        options = list(
          columnDefs = list(list(className = 'dt-right', targets = 1)),
          searching = FALSE,
          paging = FALSE,
          dom = 'ft',
          ordering = FALSE
        ),
        rownames = FALSE) %>%
      formatStyle(1:2,
                  backgroundColor = 'white')
  })

  output$performancechart <- renderPlot({
    req(performance$results)

    r <- performance$results

    r %>%
      mutate(gear = factor(gear, levels = c("1", "2", "3", "4", "5"), labels = c("1st", "2nd", "3rd", "4th", "5th"))) %>%
      ggplot(aes(speed, acceleration, group = gear)) +
      geom_point(stat = "identity", aes(color = gear), size = 1.5) +
      scale_color_manual(values = rev(c("#b3e6cc",
                                    "#79d2a6",
                                    "#40bf80",
                                    "#339966",
                                    "#206040"))) +
      # "#133926"
                                    # "#06130d"
      theme(
        legend.position = 'right',
        panel.background = element_blank(),
        axis.text = element_text(face = 'bold', size = 10),
        axis.title.x = element_text(face = 'bold', size = 11, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(face = 'bold', size = 11),
        legend.title = element_blank(),
        legend.text = element_text(face = 'bold', size = 10),
        plot.title = element_text(size=11, face = "bold")
      ) +
      xlim(0, 200) +
      ylim(0, 30) +
      # , margin = margin(t = 0, r = 15, b = 0, l = 0)
      # colour = "#333333"
      xlab("Speed (mph)") +
      ylab(NULL) +
      ggtitle("Acceleration (mph/s)")
  },
  height = 325)

  output$performancetable <- DT::renderDataTable({
    req(performance$results)

    r <- performance$results

    r %>%
      datatable(
        extensions = "Scroller",
        selection = 'single',
        options = list(
          scroller = TRUE,
          deferRender = TRUE,
          searching = FALSE,
          scrollX = TRUE,
          scrollY = 550,
          dom = 'ft',
          ordering = TRUE
        ),
        rownames = FALSE) %>%
      formatRound(1:ncol(r), 2)
  })

  output$range <- DT::renderDataTable({
    req(range$results)

    range$results %>%
      datatable(
        extensions = "Scroller",
        selection = 'single',
        options = list(
          scroller = TRUE,
          deferRender = TRUE,
          searching = FALSE,
          scrollX = TRUE,
          scrollY = 550,
          dom = 'ft',
          ordering = TRUE
        ),
        rownames = FALSE) %>%
      formatRound(1:ncol(range$results), 2)
  })

  zero60 <- reactive({
    req(performance$results)

    r <- performance$results %>%
      filter(speed < 60)

    max(r$time, na.rm = TRUE)
  })

  qtrmiletime <- reactive({
    req(performance$results)

    r <- performance$results %>%
      filter(distance_miles > .25)

    min(r$time, na.rm = TRUE)
  })

  cost <- reactive({
    motor <- motor()$Price * input$no.motors
    battery <- setup$battery_cost * input$battery.size
    other <- ((motor + battery) * setup$other_costs) / (1 - setup$other_costs)

    motor + battery + other
  })

  state <- reactive({
    querydb(q = sprintf("select * from Energy where State='%s'", input$state))
  })

  output$performance.summary <- DT::renderDataTable({
    req(performance$results)

    miles <- max(range$results$distance)

      # range$results %>%
      # group_by(Test) %>%
      # summarise(Miles = ft_miles(sum(velocity)))

    efficiency <- range$results %>%
      group_by(Test) %>%
      summarise(Motor = ftlbf_kwh(sum(motor))) %>%
      .$Motor %>%
      sum()

    gascost <- state()$Gas * (input$annual_miles / input$avgmpg)
    electriccost <- input$annual_miles / efficiency / setup$charge_efficiency * state()$Electricity

    tibble(
      `0-60 mph (s)` = formatC(zero60(), digits = 1, format = 'f'),
      `Quarter Mile Time (s)` = formatC(qtrmiletime(), digits = 1, format = 'f'),
      `Top Speed (mph)` = formatC(max(performance$results$speed, na.rm = TRUE), digits = 1, format = 'f'),
      `Efficiency (Miles/kWh)` = formatC(miles / efficiency, digits = 1, format = 'f'),
      `Estimated Driving Range (Miles)` = formatC(miles / efficiency * input$battery.size, digits = 1, format = 'f'),
      `Estimated Equipment Cost` = paste0('$', prettyNum(round(cost()), big.mark=",", add.trim = TRUE)),
      `Annual Energy Savings` = paste0('$', prettyNum(round(gascost - electriccost, 0), big.mark=",", add.trim = TRUE))) %>%
      # gather(Metric, Value, `0-60 mph (s)`:`Annual Energy Savings`) %>%
      gather(Metric, Value, `0-60 mph (s)`:`Annual Energy Savings`) %>%
      datatable(
        colnames = c("Performance Summary", ""),
        selection = 'none',
        options = list(
          columnDefs = list(list(className = 'dt-right', targets = 1)),
          searching = FALSE,
          paging = FALSE,
          dom = 'ft',
          ordering = FALSE
        ),
        rownames = FALSE) %>%
      formatStyle(1:2,
                  backgroundColor = 'white')
  })

  observeEvent(input$clear, {
    performance$results <- NULL
    updateSelectInput(session, 'car', selected = "Generic Aero Small Car-RWD")
    updateSelectInput(session, 'motor', selected = "34-96")
  })

  output$dbTable <- DT::renderDataTable({
    querydb(input$dataset) %>%
      datatable(extensions = "Scroller",
                selection = 'single',
                options = list(
                  scroller = TRUE,
                  deferRender = TRUE,
                  searching = FALSE,
                  scrollX = TRUE,
                  scrollY = 400,
                  dom = 'ft',
                  ordering = FALSE
                ),
                rownames = FALSE)
  }, server = TRUE)

  editdata <- reactive({
    index <- input$dbTable_rows_selected
    table <- input$dataset

    data <- querydb(table)[index, ]

    data %>%
      gather(Field, Value)
  })

  output$dataedit <- DT::renderDataTable({
    editdata() %>%
      datatable(
        selection = 'single',
        options = list(
          dom = 'ft',
          searching = FALSE,
          scrollX = TRUE,
          scrollY = 400
        ),
        rownames = FALSE
      )
  })

  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.

  output$editui <- renderUI({

    req(input$dataedit_rows_selected)

    if (is.na(as.numeric(editdata()[input$dataedit_rows_selected, 2]))) {
      textInput("val",
                "Value",
                editdata()[input$dataedit_rows_selected, 2])
    } else {
      numericInput("val",
                   "Value",
                   editdata()[input$dataedit_rows_selected, 2])
    }
  })
  dataModal <- function(failed = FALSE) {
    modalDialog(
      DT::dataTableOutput("dataedit"),

      conditionalPanel(
        condition = "input.dataedit_rows_selected.length > 0",
        uiOutput("editui"),
        actionButton("enter", "Enter")
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }

  observeEvent(input$edit, {
    showModal(dataModal())
  })

  dtrow <- reactiveVal(value = NA)

  observeEvent(input$dbTable_row_last_clicked, {
   dtrow(input$dbTable_row_last_clicked)
  })

  observeEvent(input$enter, {

    if (is.na(dtrow))
      return(NULL)

    db <- dbConnect(SQLite(), "data/db")

    q <- sprintf("update %s set `%s` = %s where id = %s",
            input$dataset,
            editdata()[input$dataedit_rows_selected, "Field"],
            input$val,
            dtrow())

    dbGetQuery(db, q)

    dbDisconnect(db)
  })

  observeEvent(input$ok, {
    removeModal()
  })
})