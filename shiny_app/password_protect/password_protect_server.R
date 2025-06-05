# Get credentials for securing app ----
credentials <- readRDS("password_protect/credentials.rds")


credentials<- credentials %>%
  filter(test_setting %in% test )

# Shinymanager Auth
res_auth <- secure_server(
  check_credentials = check_credentials(credentials),
  timeout = 30
)

output$auth_output <- renderPrint({
  reactiveValuesToList(res_auth)
})
