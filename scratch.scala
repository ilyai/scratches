import java.io.File

import io.outofaxis.pixelart.cms.appservice.advertisermodule.domain.model.{AppId, AssetId}
import play.api.libs.json._
import play.api.libs.json.JsValue
import play.api.libs.json.Json

val a: JsValue = Json.parse(

  """ {
     "adValue": {
       "name": "colorful event",
       "placeholders": [
         {
           "id": "bfe56988-5060-47c2-9e81-f0c8e8f3993b",
           "name": "Placeholder 1",
           "level": 0,
           "index": 0,
           "imageFit": "STRETCH",
           "transition": "Slide",
           "position": {
             "direction": "column",
             "grow": 20,
             "shrink": 0
           },
           "widgets": [
             {
               "assetIds": [
                 "AAEAAQAAAAAAAAAAAAAAJGRlZDM4NjdmLWEzYjctNDM0MS05ZjAxLWI4NmM3NTZkMzgyMw"
               ],
               "appName": "sse.png",
               "thumbnail": "assets/image-tmb.png",
               "param": {
                 "size": "contain",
                 "src": "",
                 "position": "center",
                 "fit": "",
                 "systemPreviewId": "AAEAAQAAAAAAAAAAAAAAJDI5MzA4ZThmLWQ0NDEtNGM0ZS1iMDk5LTc0OTc1MDAxNTcxYw",
                 "duration": 5000,
                 "systemSummary": "sse.png",
                 "systemPreview": "",
                 "repeat": "no-repeat",
                 "srcId": "AAEAAQAAAAAAAAAAAAAAJGRlZDM4NjdmLWEzYjctNDM0MS05ZjAxLWI4NmM3NTZkMzgyMw"
               },
               "url": "http://localhost:9000/api/1/tenant/83c460e9-f785-430a-af29-c83d19b8b6d7/appTemplates/appContent/5967ff1a-918f-4d9f-890f-257c9575e39e/",
               "contentId": "4a619dff-fdfc-4750-94ae-3506e414f990",
               "title": "sse.png",
               "icon": "picture-o",
               "id": "c8d7e55b-443a-4cb0-8044-8b8391dc6c80",
               "description": "This widget will show your picture",
               "widgetId": "5967ff1a-918f-4d9f-890f-257c9575e39e",
               "group": "image"
             }
           ]
         }
       ]
     }
          }"""

)

val b = Json.parse(
  """
    |[
    |  {
    |    "playerId": "b0284da4-c17b-48bb-bf9b-0e2045857ea1",
    |    "campaignId": {
    |      "value": "d154b004-6e30-48bb-893d-1a5ad15561d1"
    |    },
    |    "logServer": "http://b.pixelart.ge:5111/log-data/players/b0284da4-c17b-48bb-bf9b-0e2045857ea1/play-logs",
    |    "startDate": "2017-12-01T21:04:40+0400",
    |    "endDate": "2017-12-31T21:04:40+0400",
    |    "weekSchedule": [
    |      {
    |        "start": {
    |          "day": 3,
    |          "hour": 0,
    |          "quarter": 0
    |        },
    |        "end": {
    |          "day": 3,
    |          "hour": 24,
    |          "quarter": 0
    |        }
    |      },
    |      {
    |        "start": {
    |          "day": 6,
    |          "hour": 0,
    |          "quarter": 0
    |        },
    |        "end": {
    |          "day": 6,
    |          "hour": 24,
    |          "quarter": 0
    |        }
    |      },
    |      {
    |        "start": {
    |          "day": 2,
    |          "hour": 0,
    |          "quarter": 0
    |        },
    |        "end": {
    |          "day": 2,
    |          "hour": 24,
    |          "quarter": 0
    |        }
    |      },
    |      {
    |        "start": {
    |          "day": 0,
    |          "hour": 0,
    |          "quarter": 0
    |        },
    |        "end": {
    |          "day": 0,
    |          "hour": 24,
    |          "quarter": 0
    |        }
    |      },
    |      {
    |        "start": {
    |          "day": 5,
    |          "hour": 0,
    |          "quarter": 0
    |        },
    |        "end": {
    |          "day": 5,
    |          "hour": 24,
    |          "quarter": 0
    |        }
    |      },
    |      {
    |        "start": {
    |          "day": 1,
    |          "hour": 0,
    |          "quarter": 0
    |        },
    |        "end": {
    |          "day": 1,
    |          "hour": 24,
    |          "quarter": 0
    |        }
    |      },
    |      {
    |        "start": {
    |          "day": 4,
    |          "hour": 0,
    |          "quarter": 0
    |        },
    |        "end": {
    |          "day": 4,
    |          "hour": 24,
    |          "quarter": 0
    |        }
    |      }
    |    ],
    |    "ad": {
    |      "name": "tree3",
    |      "placeholders": [
    |        {
    |          "id": "39d3e646-99ed-4502-91d5-4c20ea0c17b7",
    |          "name": "Layer 1",
    |          "level": 0,
    |          "index": 0,
    |          "imageFit": "STRETCH",
    |          "transition": "Fade",
    |          "position": {
    |            "direction": "column",
    |            "grow": 20,
    |            "shrink": 0
    |          },
    |          "widgets": [
    |            {
    |              "internetRequired": true,
    |              "developerId": "admin@pxlart.io",
    |              "assetIds": [
    |                "AAIA____AAAAAQAAAAAAAAAAAAAAJDg0YmUyYTg4LTRmMzYtNDE2Ni1iYmM3LWI1ZTlhMGI1MTE1Yw"
    |              ],
    |              "thumbnail": "assets/countdown-tmb.gif",
    |              "param": {
    |                "size": "contain",
    |                "position": "center",
    |                "particles": "off",
    |                "font": "Arial",
    |                "fontcolor": "#c3fffe",
    |                "duration": 10000,
    |                "title": "Event ends in",
    |                "deadline": "2017-12-01 21:05",
    |                "repeat": "no-repeat",
    |                "internal": false,
    |                "bgcolor": "#3f455f",
    |                "srcId": "AAIA____AAAAAQAAAAAAAAAAAAAAJDg0YmUyYTg4LTRmMzYtNDE2Ni1iYmM3LWI1ZTlhMGI1MTE1Yw"
    |              },
    |              "developerTenantId": "3121cd29-7e28-4a93-953a-ffd845422757",
    |              "url": "https://b.pixelart.ge:5201/api/1/tenant/3121cd29-7e28-4a93-953a-ffd845422757/appTemplates/appContent/10206962-8c4c-4770-8abb-9c6b384b61bf/",
    |              "isInContentLibrary": false,
    |              "contentId": "50ebb316-0a3d-49d8-93a1-ea61a51bd379",
    |              "templateId": "10206962-8c4c-4770-8abb-9c6b384b61bf",
    |              "title": "Countdown",
    |              "icon": "hourglass-half",
    |              "id": "a1dc4f72-157f-4553-9a84-39a6efe60a97",
    |              "description": "This app displays a web page.",
    |              "widgetId": "50ebb316-0a3d-49d8-93a1-ea61a51bd379",
    |              "group": "image"
    |            },
    |            {
    |              "internetRequired": true,
    |              "developerId": "admin@pxlart.io",
    |              "assetIds": [
    |                "AAIA____AAxxAAAQAAAAAAAAAAAAAAJDg0YmUyYTg4LTRmMzYtNDE2Ni1iYmM3LWI1ZTlhMGI1MTE1Ywzz"
    |              ],
    |              "thumbnail": "assets/countdown-tmb.gif",
    |              "param": {
    |                "size": "contain",
    |                "position": "center",
    |                "particles": "off",
    |                "font": "Arial",
    |                "fontcolor": "#c3fffe",
    |                "duration": 10000,
    |                "title": "Event ends in",
    |                "deadline": "2017-12-01 21:05",
    |                "repeat": "no-repeat",
    |                "internal": false,
    |                "bgcolor": "#3f455f",
    |                "srcId": "AAIA____AAAAAQAAAAAAAAAAAAAAJDg0YmUyYTg4LTRmMzYtNDE2Ni1iYmM3LWI1ZTlhMGI1MTE1Yw"
    |              },
    |              "developerTenantId": "3121cd29-7e28-4a93-953a-ffd845422757",
    |              "url": "https://b.pixelart.ge:5201/api/1/tenant/3121cd29-7e28-4a93-953a-ffd845422757/appTemplates/appContent/10206962-8c4c-4770-8abb-9c6b384b61bf/",
    |              "isInContentLibrary": false,
    |              "contentId": "50ebb316-0a3d-49d8-93a1-ea61a51bd379",
    |              "templateId": "10206962-8c4c-4770-8abb-9c6b384b61bf",
    |              "title": "Countdown",
    |              "icon": "hourglass-half",
    |              "id": "a1dc4f72-157f-4553-9a84-39a6efe60a97",
    |              "description": "This app displays a web page.",
    |              "widgetId": "50ebb316-0a3d-49d8-93a1-ea61a51bd379",
    |              "group": "image"
    |            }
    |          ]
    |        }
    |      ]
    |    }
    |  }
    |]
  """.stripMargin)


//(a \ "aaaa")
//(a \\ "aaaa").map(_.asOpt[Set[String]])
//
//(a \\ "widgets").map(_ \\ "id").flatten.map(v => AppId(v.toString))
//val b = (a \\ "widgets").map(_ \\ "contentId").flatten.map(v => AppId(v.as[String]))

//val path = getClass.getResource("appTemplates").getPath
//val f = new File(path)
//f.listFiles.filter(_.isFile).toList

(a \ "adValue" \ "name").getOrElse(JsString("foobar")).as[String]
"../.appstore/bf4ced81-28be-4c84-a49b-581d1e379f64/settings.html".matches(".*settings.html.*")

//(b \\ "assetIds").map(_.asOpt[Set[String]]).flatten.headOption.getOrElse(Set())
//Option((b \\ "assetIds").flatMap(_.asOpt[Set[String]]).flatten.map(AssetId(_)).toSet).filter(_.nonEmpty).getOrElse(Set(AssetId("default")))
Option((b \\ "assetIds").flatMap(_.asOpt[Set[String]]).flatten.map(AssetId(_)).toSet).filter(_.nonEmpty).getOrElse(allAppViews.map(_.app.assetIds).flatten)
//(b \\ "assetIds").flatMap(_.asOpt[Set[String]]).headOption.map(_.map(AssetId(_))).getOrElse(Set.empty[AssetId])
(b \\ "assetIds").flatMap(_.asOpt[Set[String]]).headOption.map(_.map(AssetId(_))).getOrElse(Set.empty[AssetId])


