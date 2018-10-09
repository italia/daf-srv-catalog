import play.api.libs.json._
import play.api.libs.ws.WSAuthScheme
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import catalog_manager.yaml.{Error, MetaCatalog}
import play.api.libs.ws._
import play.api.libs.ws.ahc.AhcWSClient

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


/*
private def extractSeparator(hiveFormat :String) : Option[String] =  {
  val sep = """'separatorChar'.=.'.*'.,'""".r.findFirstIn(hiveFormat).getOrElse(",").split(" ,")(0).replace("""\\\\""", "").replaceAll("'", "").split(" = ").last.trim
  Option(sep)
}

val inferKylo = """{\"name\":null,\"description\":null,\"charset\":\"UTF-8\",\"properties\":{},\"fields\":[{\"sampleValues\":[\"2016\",\"2016\",\"2016\",\"2016\",\"2016\",\"2016\",\"2016\",\"2016\",\"2016\"],\"name\":\"DATA\",\"description\":\"\",\"nativeDataType\":null,\"derivedDataType\":\"int\",\"primaryKey\":false,\"nullable\":true,\"modifiable\":true,\"dataTypeDescriptor\":{\"numeric\":true,\"date\":false,\"complex\":false},\"updatedTracker\":false,\"precisionScale\":null,\"createdTracker\":false,\"tags\":null,\"dataTypeWithPrecisionAndScale\":\"int\",\"descriptionWithoutNewLines\":\"\"},{\"sampleValues\":[\"01\",\"01\",\"01\",\"01\",\"01\",\"01\",\"01\",\"01\",\"01\"],\"name\":\"CODICE_REGIONE\",\"description\":\"\",\"nativeDataType\":null,\"derivedDataType\":\"int\",\"primaryKey\":false,\"nullable\":true,\"modifiable\":true,\"dataTypeDescriptor\":{\"numeric\":true,\"date\":false,\"complex\":false},\"updatedTracker\":false,\"precisionScale\":null,\"createdTracker\":false,\"tags\":null,\"dataTypeWithPrecisionAndScale\":\"int\",\"descriptionWithoutNewLines\":\"\"},{\"sampleValues\":[\"Piemonte\",\"Piemonte\",\"Piemonte\",\"Piemonte\",\"Piemonte\",\"Piemonte\",\"Piemonte\",\"Piemonte\",\"Piemonte\"],\"name\":\"NOME_REGIONE\",\"description\":\"\",\"nativeDataType\":null,\"derivedDataType\":\"string\",\"primaryKey\":false,\"nullable\":true,\"modifiable\":true,\"dataTypeDescriptor\":{\"numeric\":false,\"date\":false,\"complex\":false},\"updatedTracker\":false,\"precisionScale\":null,\"createdTracker\":false,\"tags\":null,\"dataTypeWithPrecisionAndScale\":\"string\",\"descriptionWithoutNewLines\":\"\"},{\"sampleValues\":[\"111\",\"112\",\"113\",\"114\",\"115\",\"116\",\"117\",\"118\",\"119\"],\"name\":\"CODICE_TIPOLOGIA_PERSONALE\",\"description\":\"\",\"nativeDataType\":null,\"derivedDataType\":\"int\",\"primaryKey\":false,\"nullable\":true,\"modifiable\":true,\"dataTypeDescriptor\":{\"numeric\":true,\"date\":false,\"complex\":false},\"updatedTracker\":false,\"precisionScale\":null,\"createdTracker\":false,\"tags\":null,\"dataTypeWithPrecisionAndScale\":\"int\",\"descriptionWithoutNewLines\":\"\"},{\"sampleValues\":[\"Dipendente dall'Ente a tempo determinato maschio - numero\",\"Dipendente dall'Ente a tempo determinato femmina - numero\",\"Dipendente dall'Ente a tempo determinato maschio - mesi lavorati\",\"Dipendente dall'Ente a tempo determinato femmina - mesi lavorati\",\"Dipendente dalla Regione a tempo determinato maschio - numero\",\"Dipendente dalla Regione a tempo determinato femmina - numero\",\"Dipendente dalla Regione a tempo determinato maschio - mesi lavorati\",\"Dipendente dalla Regione a tempo determinato femmina - mesi lavorati\",\"Dipendente dall'ateneo a tempo determinato maschio - numero\"],\"name\":\"DESCRIZIONE_TIPOLOGIA_PERSONALE\",\"description\":\"\",\"nativeDataType\":null,\"derivedDataType\":\"string\",\"primaryKey\":false,\"nullable\":true,\"modifiable\":true,\"dataTypeDescriptor\":{\"numeric\":false,\"date\":false,\"complex\":false},\"updatedTracker\":false,\"precisionScale\":null,\"createdTracker\":false,\"tags\":null,\"dataTypeWithPrecisionAndScale\":\"string\",\"descriptionWithoutNewLines\":\"\"},{\"sampleValues\":[\"0\",\"0\",\"0\",\"0\",\"0\",\"0\",\"0\",\"0\",\"0\"],\"name\":\"NUMERO\",\"description\":\"\",\"nativeDataType\":null,\"derivedDataType\":\"int\",\"primaryKey\":false,\"nullable\":true,\"modifiable\":true,\"dataTypeDescriptor\":{\"numeric\":true,\"date\":false,\"complex\":false},\"updatedTracker\":false,\"precisionScale\":null,\"createdTracker\":false,\"tags\":null,\"dataTypeWithPrecisionAndScale\":\"int\",\"descriptionWithoutNewLines\":\"\"}],\"schemaName\":null,\"databaseName\":null,\"hiveFormat\":\"ROW FORMAT SERDE 'org.apache.hadoop.hive.serde2.OpenCSVSerde'\\n WITH SERDEPROPERTIES ( 'separatorChar' = '\\\\;' ,'escapeChar' = '\\\\\\\\' ,'quoteChar' = '\\\\\\\"') STORED AS TEXTFILE\",\"structured\":false,\"id\":\"37f125c9-b29a-429d-9291-3bc85859a2fa\"}"""

// val test = Json.parse(inferKylo)

val sep = extractSeparator(inferKylo)





val categoriesString = """[ { "owner": { "displayName": "Data Lake Administrator", "email": null, "enabled": true, "groups": [ "admin", "user" ], "systemName": "dladmin" }, "allowedActions": { "actions": [ { "systemName": "accessCategory", "actions": [ { "systemName": "editCategorySummary" }, { "systemName": "accessCategoryDetails", "actions": [ { "systemName": "editCategoryDetails" }, { "systemName": "deleteCategory" } ] }, { "systemName": "createFeedUnderCategory" }, { "systemName": "changeCategoryPermissions" } ] } ] }, "roleMemberships": [], "feedRoleMemberships": [], "id": "7a05d086-70e6-4c3b-b53c-a209bde07902", "name": "Open Data", "systemName": "open_data", "icon": "assessment", "iconColor": "#66BB6A", "description": null, "securityGroups": [], "userFields": [], "userProperties": [], "relatedFeeds": 0, "createDate": 1515425000995, "updateDate": 1515425216116 }, { "owner": { "displayName": "Data Lake Administrator", "email": null, "enabled": true, "groups": [ "admin", "user" ], "systemName": "dladmin" }, "allowedActions": { "actions": [ { "systemName": "accessCategory", "actions": [ { "systemName": "editCategorySummary" }, { "systemName": "accessCategoryDetails", "actions": [ { "systemName": "editCategoryDetails" }, { "systemName": "deleteCategory" } ] }, { "systemName": "createFeedUnderCategory" }, { "systemName": "changeCategoryPermissions" } ] } ] }, "roleMemberships": [], "feedRoleMemberships": [], "id": "3f776894-fd49-4865-a55c-80e60403da9f", "name": "roma_test", "systemName": "roma_test", "icon": null, "iconColor": null, "description": null, "securityGroups": [], "userFields": [], "userProperties": [], "relatedFeeds": 0, "createDate": 1516214069556, "updateDate": 1516214069556 }, { "owner": { "displayName": "Data Lake Administrator", "email": null, "enabled": true, "groups": [ "admin", "user" ], "systemName": "dladmin" }, "allowedActions": { "actions": [ { "systemName": "accessCategory", "actions": [ { "systemName": "editCategorySummary" }, { "systemName": "accessCategoryDetails", "actions": [ { "systemName": "editCategoryDetails" }, { "systemName": "deleteCategory" } ] }, { "systemName": "createFeedUnderCategory" }, { "systemName": "changeCategoryPermissions" } ] } ] }, "roleMemberships": [], "feedRoleMemberships": [], "id": "a021ba49-68b4-4813-b0af-a8b18f591ccd", "name": "test_json", "systemName": "test_json", "icon": null, "iconColor": null, "description": null, "securityGroups": [], "userFields": [], "userProperties": [], "relatedFeeds": 0, "createDate": 1515668890987, "updateDate": 1516202943161 }, { "owner": { "displayName": "Data Lake Administrator", "email": null, "enabled": true, "groups": [ "admin", "user" ], "systemName": "dladmin" }, "allowedActions": { "actions": [ { "systemName": "accessCategory", "actions": [ { "systemName": "editCategorySummary" }, { "systemName": "accessCategoryDetails", "actions": [ { "systemName": "editCategoryDetails" }, { "systemName": "deleteCategory" } ] }, { "systemName": "createFeedUnderCategory" }, { "systemName": "changeCategoryPermissions" } ] } ] }, "roleMemberships": [], "feedRoleMemberships": [], "id": "6ef0ef5b-5c8f-42fc-9f0d-37f67430f1f5", "name": "default_org", "systemName": "default_org", "icon": "brightness_5", "iconColor": "#FF8A65", "description": null, "securityGroups": [], "userFields": [], "userProperties": [], "relatedFeeds": 0, "createDate": 1514977170821, "updateDate": 1516214196551 }, { "owner": { "displayName": "Data Lake Administrator", "email": null, "enabled": true, "groups": [ "admin", "user" ], "systemName": "dladmin" }, "allowedActions": { "actions": [ { "systemName": "accessCategory", "actions": [ { "systemName": "editCategorySummary" }, { "systemName": "accessCategoryDetails", "actions": [ { "systemName": "editCategoryDetails" }, { "systemName": "deleteCategory" } ] }, { "systemName": "createFeedUnderCategory" }, { "systemName": "changeCategoryPermissions" } ] } ] }, "roleMemberships": [], "feedRoleMemberships": [], "id": "ac148153-e54f-41f6-b9f4-f1c190eb0d5f", "name": "System", "systemName": "system", "icon": "cloud", "iconColor": "#FFCA28", "description": "System Data", "securityGroups": [], "userFields": [], "userProperties": [], "relatedFeeds": 0, "createDate": 1512127244328, "updateDate": 1512127246778 }, { "owner": { "displayName": "Data Lake Administrator", "email": null, "enabled": true, "groups": [ "admin", "user" ], "systemName": "dladmin" }, "allowedActions": { "actions": [ { "systemName": "accessCategory", "actions": [ { "systemName": "editCategorySummary" }, { "systemName": "accessCategoryDetails", "actions": [ { "systemName": "editCategoryDetails" }, { "systemName": "deleteCategory" } ] }, { "systemName": "createFeedUnderCategory" }, { "systemName": "changeCategoryPermissions" } ] } ] }, "roleMemberships": [], "feedRoleMemberships": [], "id": "a453f503-d3b8-43ff-a43a-7ac888746b52", "name": "pac_agenziaentrate", "systemName": "pac_agenziaentrate", "icon": "account_balance_wallet", "iconColor": "#FF5252", "description": "pubblica amministrazione agenzia delle entrate", "securityGroups": [], "userFields": [], "userProperties": [], "relatedFeeds": 0, "createDate": 1515510352934, "updateDate": 1515512167365 }, { "owner": { "displayName": "Data Lake Administrator", "email": null, "enabled": true, "groups": [ "admin", "user" ], "systemName": "dladmin" }, "allowedActions": { "actions": [ { "systemName": "accessCategory", "actions": [ { "systemName": "editCategorySummary" }, { "systemName": "accessCategoryDetails", "actions": [ { "systemName": "editCategoryDetails" }, { "systemName": "deleteCategory" } ] }, { "systemName": "createFeedUnderCategory" }, { "systemName": "changeCategoryPermissions" } ] } ] }, "roleMemberships": [], "feedRoleMemberships": [], "id": "f8209368-54dc-4ba2-874a-900ac4ec5978", "name": "pac_cdc", "systemName": "pac_cdc", "icon": "image_aspect_ratio", "iconColor": "#66BB6A", "description": "pac_cdc", "securityGroups": [], "userFields": [], "userProperties": [], "relatedFeeds": 0, "createDate": 1513868513661, "updateDate": 1515514382193 } ]"""

val categoriesJson = Json.parse(categoriesString)

val categories = categoriesJson.as[List[JsValue]]
val found =categories.filter(cat => {(cat \ "systemName").as[String].equals("roma_test")})
(found.head \ "id").as[String]
*/

