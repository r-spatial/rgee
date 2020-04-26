library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

serverString <- ee$String("I am not a String!")
print(class(serverString))

clientString <- "I am a String"
print(class(clientString))

serverList <- ee$List$sequence(0, 7)
print(serverList$getInfo())

serverList <- serverList$map(
  ee_pyfunc(
    function(n) {
      ee$Number(n)$add(1)
    }
  )
)

print(serverList$getInfo())
