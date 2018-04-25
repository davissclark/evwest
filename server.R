drive.efficiency <- function(power, max.dt.loss) {
  (power - max.dt.loss) / power
}

performance.simulation <- function(time_step = 0, gear = 1, car, velocity = 0, motor, no.motors, wheel_radius, drive.efficiency,
                                   aero_frontal_area, aero_drag_coeff, rho, tire_pressure, design.mass) {

  speed <- getSpeed(velocity)
  gearratio <- getGearRatio(car, gear)
  motortorque <- getMotorTorque(motor, no.motors)
  wheeltorque <- getWheelTorque(motortorque, gearratio, getGearRatio(car, 6))
  wheelforce_gross <- getWheelForce_gross(wheeltorque, wheel_radius)
  dtforce <- getDTForce(wheelforce_gross, drive.efficiency)
  aeroforce <- getAeroForce(aero_frontal_area, aero_drag_coeff, velocity, rho)
  rollingforce <- getRollingForce(tire_pressure, speed, design.mass)
  wheelforce_net <- getWheelForce_net(wheelforce_gross, dtforce, aeroforce, rollingforce)
  acceleration <- getAcceleration(wheelforce_net, design.mass, gear)

  data.frame(time = time_step,
             gear = gear,
             gearratio = gearratio,
             velocity0 = velocity,
             velocity = velocity,
             speed = speed,
             motorrpm = 0,
             motorrpmpt = 0,
             motortorque = motortorque,
             wheeltorque = wheeltorque,
             wheelforce.gross = wheelforce_gross,
             dtforce = dtforce,
             aeroforce = aeroforce,
             rollingforce = rollingforce,
             wheelforce.net = wheelforce_net,
             acceleration = acceleration,
             distance_ft = 0,
             distance_miles = 0,
             hp = 0)
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
    car = querydb("Cars")[1, 1],
    motor = querydb("Motors")[1, 1]
  )

  observeEvent(input$car, {
    config$car <- input$car
  })

  observeEvent(input$motor, {
    config$motor <- input$motor
  })

  # output$mytabs <- renderUI({
  #   if (length(isolate(input$run)) > 0) {
  #     if (input$run > 0) {
  #       tabsetPanel(id = "viewbar",
  #                   tabPanel("Configure",
  #                            value = "config"),
  #                   tabPanel("Performance",
  #                            value = "performance"),
  #                   tabPanel("Range",
  #                            value = "range"),
  #                   tabPanel(icon("database"),
  #                            value = "database")
  #       )
  #     } else {
  #       tabsetPanel(id = "viewbar",
  #                   tabPanel("Configure",
  #                            value = "config"),
  #                   tabPanel(icon("database"),
  #                            value = "database")
  #       )
  #     }
  #   } else {
  #     tabsetPanel(id = "viewbar",
  #       tabPanel("Configure",
  #                value = "config"),
  #       tabPanel(icon("database"),
  #                value = "database")
  #     )
  #   }
  # })

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
                                        choices = c("", querydb("Cars")[, 1]),
                                        selected = ifelse(!is.null(config$car), config$car, ""))
                        ),
                        div(
                          class = "tools",
                          fluidRow(
                            column(8,
                                   selectInput("motor",
                                               label = "Motor Type",
                                               choices = c("", querydb("Motors")[, 1]),
                                               selected = ifelse(!is.null(config$motor), config$motor, ""))
                            ),
                            column(4,
                                   numericInput("no.motors",
                                                "# Motors", 1,
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
                             class = 'details',
                             DT::dataTableOutput("performance.summary"),
                             br(),
                             br(),
                             br(),
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
             DT::dataTableOutput("tablelist")
             # selectInput("dataset",
             #             "Select table",
             #             tablelistdb(),
             #             selected = "UpdateLog")
      ),
      column(9,
             conditionalPanel(
               condition = 'input.dbTable_rows_selected.length > 0',
               actionLink("edit", "Edit")
             ),
             DT::dataTableOutput("dbTable")
      )
    ))
    )
  })

  output$tablelist <- DT::renderDataTable({
    tablelistdb() %>%
      as.data.frame() %>%
      datatable(extensions = "Scroller",
                selection = list(mode = 'single', selected = 1, target = "row"),
                colnames = c("Select Table"),
                options = list(
                  scroller = TRUE,
                  deferRender = TRUE,
                  searching = FALSE,
                  scrollX = TRUE,
                  dom = 'ft',
                  ordering = FALSE
                ),
                rownames = FALSE)
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

  # observe({
  #   defaults$aero_drive_type <- car()$Drive
  # })

  observe({
    defaults$aero_frontal_area <- car()$Ft2.20
  })

  observe({
    defaults$aero_drag_coeff <- car()$Cd
  })
#
#   observe({
#     defaults$tire_width <- car()$Width.Side.Wall..mm.
#   })

  # observe({
  #   defaults$tire_section_height <- car()$Aspect.Side.Wall..mm.
  # })

  # observe({
  #   defaults$tire_rim <- car()$Rim.Radius..in.
  # })

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
    mass$engine_wt_removed <- setup$engine_carwt * car()$Lbsm
  })

  dt.loss <- reactive({
    switch(car()$Drive,
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

  de <- reactive({
    drive.efficiency(power(), max.dt.loss())
  })

  wheelRadius <- reactive({

    getWheelRadius(car()$Width.Side.Wall..mm.,
                   car()$Aspect.Side.Wall..mm.,
                   car()$Rim.Radius..in.)
    # getWheelRadius(defaults$tire_width,
    #                defaults$tire_section_height,
    #                defaults$tire_rim)
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

      round <- performance.simulation(time_step,
                                      gear = 1,
                                      input$car,
                                      0,
                                      motor(),
                                      input$no.motors,
                                      wheelRadius(),
                                      de(),
                                      defaults$aero_frontal_area,
                                      defaults$aero_drag_coeff,
                                      setup$rho,
                                      setup$tire_pressure,
                                      design.mass())

      round_alt <- performance.simulation(time_step,
                                          gear = 2,
                                          input$car,
                                          0,
                                          motor(),
                                          input$no.motors,
                                          wheelRadius(),
                                          de(),
                                          defaults$aero_frontal_area,
                                          defaults$aero_drag_coeff,
                                          setup$rho,
                                          setup$tire_pressure,
                                          design.mass())

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
                                        de())

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

      round_alt[i, "motorrpmpt"] <- ifelse(round_alt[i, "motorrpm"] > getMotorLimit(motor()),
                                           getMotorLimit(motor()) / 1000,
                                           round_alt[i, "motorrpm"] / 1000)

      round_alt[i, "motortorque"] <- getMotorTorque(motor(),
                                                    input$no.motors,
                                                    round_alt[i, "motorrpmpt"])

      round_alt[i, "wheeltorque"] <- getWheelTorque(round_alt[i, "motortorque"],
                                                    round_alt[i, "gearratio"],
                                                    getGearRatio(input$car, 6))

      round_alt[i, "wheelforce.gross"] <- getWheelForce_gross(round_alt[i, "wheeltorque"], wheelRadius())
      round_alt[i, "dtforce"] <- getDTForce(round_alt[i, "wheelforce.gross"], de())
      round_alt[i, "aeroforce"] <- getAeroForce(defaults$aero_frontal_area, defaults$aero_drag_coeff, round_alt[i, "velocity"], setup$rho)
      round_alt[i, "rollingforce"] <- getRollingForce(setup$tire_pressure, round_alt[i, "speed"], design.mass())
      round_alt[i, "wheelforce.net"] <- getWheelForce_net(round_alt[i, "wheelforce.gross"], round_alt[i, "dtforce"], round_alt[i, "aeroforce"], round_alt[i, "rollingforce"])

      round_alt[i, "acceleration"] <- getAcceleration(round_alt[i, "wheelforce.net"],
                                                      design.mass(),
                                                      gear_alt)
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
      incProgress(1)

      rounds <- 1:nrow(range$simulation)

      r <- range$simulation %>%
        mutate(velocity = mph_fps2(MPH))

      r[, "distance"] <- cumsum(sapply(rounds, function(x) {
        ifelse(x == 1, 0, (r[x, "velocity"] + r[x-1, "velocity"]) / 2 / 5280)
      }, USE.NAMES = FALSE))

      r[, "acceleration"] <- sapply(rounds, function(x) {
        ifelse(x == 1, 0, r[x, "velocity"] - r[x-1, "velocity"])
      }, USE.NAMES = FALSE)

      r[, "gear"] <- sapply(rounds, function(x) {
        performance$results %>%
          filter(velocity <= r[x, "velocity"]) %>%
          .$gear %>%
          max(na.rm = TRUE)
      }, USE.NAMES = FALSE)

      range$results <- r %>%
        mutate(mass = getRotationalMass(gear) * design.mass()) %>%
        mutate(inertial = lbf_slugs(mass) * acceleration) %>%
        mutate(aero = getAeroForce(defaults$aero_frontal_area, defaults$aero_drag_coeff, velocity, setup$rho)) %>%
        mutate(rolling = if_else(velocity > 0, getRollingForce(setup$tire_pressure, MPH, design.mass()), 0)) %>%
        mutate(dt = abs(inertial + aero + rolling) * (1 - de())) %>%
        mutate(motor = if_else((inertial + aero + rolling + dt) >= 0,
                               (inertial + aero + rolling + dt) / setup$motor_efficiency,
                               (inertial + aero + rolling + dt) * setup$regeneration)) %>%
        mutate(inertial = inertial * velocity,
               aero = aero * velocity,
               rolling = rolling * velocity,
               dt = dt * velocity,
               motor = motor * velocity)
    })
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
      select(Car15,
             `Drag Coefficient (Cd)` = Cd,
             `Frontal Area (sqft)` = `Ft2.20`,
             `Design Mass (lbm)`) %>%
      gather(Car, Value, Car15:`Design Mass (lbm)`) %>%
      mutate(Value = ifelse(Car == "Frontal Area (sqft)",
                            formatC(as.numeric(Value),
                                    digits = 1,
                                    format = 'f'),
                            formatC(as.numeric(Value),
                                    digits = 2,
                                    format = 'f'))) %>%
      filter(Car != "Car15")

    d[3, 2] <- prettyNum(round(as.numeric(d[3, 2]), 0),
                         big.mark = ',')

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
      mutate(Value = formatC(as.numeric(Value),
                             digits = 2,
                             format = 'f'))

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

    d <- data.frame(width = car()$Width.Side.Wall..mm.,
               sheight = car()$Aspect.Side.Wall..mm.,
               rim = car()$Rim.Radius..in.,
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
      mutate(Value = formatC(as.numeric(Value),
                             digits = 1,
                             format = 'f')) %>%
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
      mutate(gear = factor(gear,
                           levels = c("1", "2", "3", "4", "5"),
                           labels = c("1st", "2nd", "3rd", "4th", "5th"))) %>%
      ggplot(aes(speed,
                 velocity,
                 group = gear)) +
      geom_point(stat = "identity",
                 aes(color = gear),
                 size = 1.5) +
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
        axis.text = element_text(face = 'bold',
                                 size = 10),
        axis.title.x = element_text(face = 'bold',
                                    size = 11,
                                    margin = margin(t = 15,
                                                    r = 0,
                                                    b = 0,
                                                    l = 0)),
        axis.title.y = element_text(face = 'bold',
                                    size = 11),
        legend.title = element_blank(),
        legend.text = element_text(face = 'bold',
                                   size = 10),
        plot.title = element_text(size=11,
                                  face = "bold")
      ) +
      xlab("Speed (mph)") +
      ylab(NULL) +
      ggtitle("Velocity (mph/s)")
  },
  height = 325)

  output$performancetable <- DT::renderDataTable({
    req(performance$results, range$results)

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
    req(performance$results, range$results)

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
    updateSelectInput(session, 'car', selected = querydb("Cars")[1, 1])
    updateSelectInput(session, 'motor', selected = querydb("Motors")[1, 1])
  })

  vals <- reactiveValues(d = NULL)

  dataset <- reactive({
    tablelistdb()[input$tablelist_rows_selected]
  })

  observe({
    req(input$tablelist_rows_selected)
    vals$d <- querydb(dataset())
  })

  observeEvent(input$ok, {
    vals$d <- querydb(dataset())
  },
  ignoreInit = TRUE)


  output$dbTable <- DT::renderDataTable({
    req(vals$d)

    vals$d  %>%
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

  edit <- reactiveValues(d = NULL)

  observe({
    req(input$dbTable_rows_selected, input$tablelist_rows_selected)
    index <- input$dbTable_rows_selected
    table <- dataset()

    edit$d <- querydb(table)[index, ] %>%
      gather(Field, Value)
  })

  output$dataedit <- DT::renderDataTable({
    req(edit$d)

    edit$d %>%
      datatable(
        selection = 'single',
        options = list(
          dom = 'ft',
          searching = FALSE,
          scrollX = TRUE,
          scrollY = 400,
          paging = FALSE
        )
        # rownames = FALSE
      )
  })



  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.

  output$editui <- renderUI({

    req(input$dataedit_rows_selected)

    if (is.na(edit$d[input$dataedit_rows_selected, 2])) {
      NULL
    } else if (is.na(as.numeric(edit$d[input$dataedit_rows_selected, 2]))) {
      textInput("val",
                "Value",
                edit$d[input$dataedit_rows_selected, 2])
    } else {
      numericInput("val",
                   "Value",
                   edit$d[input$dataedit_rows_selected, 2])
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

  # dtrow <- reactiveVal(value = NA)

  # observeEvent(input$dbTable_row_last_clicked, {
  #  dtrow(input$dbTable_row_last_clicked)
  # })

  observeEvent(input$enter, {

    # if (is.na(dtrow()))
    #   return(NULL)

    db <- dbConnect(SQLite(), "data/db")

    col <- edit$d[input$dataedit_rows_selected, 1]

    q <- sprintf("update %s set `%s` = '%s' where id = '%s';",
            dataset(),
            col,
            input$val,
            input$dbTable_rows_selected)

    dbGetQuery(db, q)

    dbDisconnect(db)
  })

  observeEvent(input$enter, {
    edit$d[input$dataedit_rows_selected, 2] <- input$val
  })

  observeEvent(input$ok, {
    removeModal()
  })
})