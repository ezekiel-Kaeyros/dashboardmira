box::use(
  mongolite[mongo]
  )

fetch_mongodb <- function(connection_string, collection, db) {
  connection_string = "mongodb+srv://miq-user:Wy1N4zclOtlnR64d@miq-db.ppexwp3.mongodb.net/miq-db"
  reports <- mongo(collection=collection, db=db, url=connection_string)
  reports <- as.data.frame(reports$find())
}
