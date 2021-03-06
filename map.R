  library(mapdata)
  library(maptools)
  library(ggplot2)
  library(plyr)
  require(rgdal) 
  require(gpclib)
  library(dplyr)
  library(RODBC)
  
  # Database Connection
  db = odbcConnect("mysql_server_64", uid="root", pwd="")
  sqlQuery(db, "USE ma_charity_small")
  
  # gpclib Package Permission
  gpclibPermit()
  
  # Read downloaded .shp map 
  (Source: https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/#)  
  france_map = readOGR("C://Users//fengx//Downloads//communes-20210101-shp (1)", "communes-20210101")
  
  # Map data setting and restrict to france metropolitan 
  france_map@data$id = rownames(france_map@data)
  france_map.points = fortify(france_map, region="id")
  france_map.df = join(france_map.points, france_map@data, by="id")
  france_map.df = filter(france_map.df, lat > 40.612089 & lat < 51.122672 & long > -7.917282 & long < 10.891311)
  
  # Generate data frame of donation amounts by code_geo of donors
  query = "SELECT code_geo AS insee,
       SUM(CASE WHEN (CASE WHEN MONTH(a.act_date) <= 6 THEN YEAR(a.act_date) ELSE YEAR(a.act_date) + 1 END) = '2018' THEN a.amount ELSE 0 END) AS MAT2018
  FROM contacts AS c
  LEFT JOIN acts AS a
  ON c.id = a.contact_id
  GROUP BY insee;"
  
  q = sqlQuery(db, query)  
  
  # Keep MAT2018
  q2 = filter(q, MAT2018 != 0)
  
  # Find quartiles to slice the data
  quartiles = quantile(q2$MAT2018)
  
  # Set donation data
  q2 = mutate(q2, Amount_Quartiles = cut(MAT2018, quartiles)) %>%
       filter(!is.na(Amount_Quartiles)) %>%
       mutate(Amount_Quartiles = factor(Amount_Quartiles, labels = c('0 - 40', '40 - 90', '90 - 190', '190 - 20269.57'))) 

  # Join the tables based on id: insee (code_geo)
  france_map.df = left_join(france_map.df, q2)
   
  # Plotting
  gr = ggplot(france_map.df, aes(x=long, y=lat, group = insee, fill = Amount_Quartiles)) + 
    geom_polygon() +
    coord_map('polyconic') + 
    scale_fill_manual(values=c("#00bdcd","#ffbc14","#f88421","#ef1828")) +
    theme(
      panel.grid=element_blank(),
      panel.background=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_blank())
  
  print(gr)

  # Close Connection
  odbcClose(db)

  
