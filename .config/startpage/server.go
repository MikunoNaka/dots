package main

import (
	"github.com/gin-gonic/gin"
	"net/http"
	"time"
)

func main() {
	router := gin.New()
	router.LoadHTMLGlob("/home/zt/.config/startpage/index.html")
	router.Static("main", "/home/zt/.config/startpage/")

	router.GET("/", func (c *gin.Context) {
		time := time.Now().Format("15:04 02 Jan (Monday)")
		c.HTML(http.StatusOK, "index.html", gin.H{
			"time": time,
		})
	})

	router.Run(":8081")
}
