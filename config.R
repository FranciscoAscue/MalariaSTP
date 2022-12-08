### local host and port
# options( shiny.host = '192.168.5.186' )
# options( shiny.port = 8585 )
#options( shiny.maxRequestSize = 100*1024^2 ) 

user_base <- tibble::tibble(
  user = c("admin","brigada1","brigada2","brigada3"),
  password = sapply(c("satipored2022", "123brigada001","231brigada002","321brigada003"),
                    sodium::password_store),
  permissions = c("admin","standard","standard","standard"),
  name = c("ADMINISTRADOR", "BRIGADA1", "BRIGADA2","BRIGADA3")
)

USER_MYSQL <- "rissatip_admin"
PASSWORD_MYSQL <- "satipored2022@@%%&&"
HOST_MYSQL <- '157.90.212.15'
PORT_MYSQL <- 3306
DB_MYSQL <- 'rissatip_dbsatipo'


# USER_MYSQL <- "admin"
# PASSWORD_MYSQL <- "satipored2022"
# HOST_MYSQL <- 'localhost'
# PORT_MYSQL <- 3306
# DB_MYSQL <- 'dbsatipo'
