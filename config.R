### local host and port
options( shiny.host = '127.0.0.1' )
options( shiny.port = 8585 )
#options( shiny.maxRequestSize = 100*1024^2 ) 

user_base <- tibble::tibble(
  user = c("admin","brigada"),
  password = sapply(c("satipored2022", "123brgd001"), sodium::password_store),
  permissions = c("admin", "admin"),
  name = c("ADMINISTRADOR", "VERONICA")
)
