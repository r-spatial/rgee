# Creating a valid Service Account key for rgee

## 1. Enter to google cloud [service-account-key](https://console.cloud.google.com/apis/credentials/serviceaccountkey).

![](p1.png)

## 2. Put a name for your service account key e.g. GCS_AUTH_FILE.json (but any name will be fine).

![](p2.png)

## 3. Add GCS admin privileges to the service account key.

![](p3.png)

## 4. Create the key.

![](p4.png)

## 5. Save the credential in the folder: rgee::ee_get_earthengine_path()

![](p5.png)

## 6. PSS! You can use the same service account key for multiple users! ... but first give permision to the users :) (Go to Bucket details)

![](rgee_add_1.png)

## 7. Click in permissions and add

![](rgee_add_2.png)

## 8. Add a new menber a give privileges to manipulate GCS resources.

![](rgee_add_3.png)

## 9. Click in save

![](rgee_add_4.png)

## 10. That's all now ee_Initialize(gcs=TRUE) will work with no problems!

![](rgee_add_5.png)
