package controllers.modules

import java.io.FileInputStream
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import instance.ConfigurationInstance
import it.gov.daf.catalogmanager.catalog.MongoRepository
import org.mockito.Mockito.mock
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FunSuite}
import it.gov.daf.model.{DatasetNameFields, Error, MetaCatalog}
import io.circe.generic.auto._
import play.Environment
import play.api.libs.json.Json
import io.circe.parser._
import org.junit.Assert._
import play.api.Logger
import play.api.libs.ws.WSClient
import scala.concurrent.ExecutionContext.Implicits._

class MongoRepositoryTest extends FunSuite
  with ConfigurationInstance
  with BeforeAndAfter
  with MongoEmbedDatabase
  with BeforeAndAfterAll{
  private def withController[U](f: MongoRepository => U) = f {new TestMongoDB}

  private val logger = Logger(this.getClass.getName)
  val testMongoDB = new TestMongoDB

  var mongoInstance: MongodProps = null
  // Start In-memory Mongo instance in beforeAll statement
  override protected def beforeAll() = {
    //Starting mongo on this default port
    mongoStart(27017)
  }

  override protected def afterAll(): Unit = {
    mongoStop(mongoInstance)
  }

  after {
    testMongoDB.collection.drop()
  }

  test("def isPresent") {
    val jsonStreamFile = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJson.json"))
    val jsonMeta = try { Json.parse(jsonStreamFile) } finally { jsonStreamFile.close() }
    val met = decode[MetaCatalog](jsonMeta.toString())
    logger.debug("\ndef isPresent\nFile json = /test/FileJson/TestJson.json\n\nI aspected false and the return is:\n" + testMongoDB.isPresent("bandi_test_consip"))
    assertTrue("test metacatalog is not present",!testMongoDB.isPresent("bandi_test_consip"))
    met match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => logger.debug("Success decode"); testMongoDB.addMetaCatalog(metacatalog)
    }
    logger.debug("\ndef isPresent\nFile json = /test/FileJson/TestJson.json\n\nI aspected true and the return is:\n" + testMongoDB.isPresent("bandi_test_consip"))
    assertTrue("test metacatalog is present",testMongoDB.isPresent("bandi_test_consip"))
  }

  test("def getPublicMetaCatalogByName"){
    val testEmpty = testMongoDB.getPublicMetaCatalogByName("bandi_test_consip")
    testEmpty map { x =>
      logger.debug("\ndef getPublicMetaCatalogByName\n\nDatabase is empty\nI aspected 404 and the return is:\n" + x)
      assertTrue("Not found", x.left.get.code.get == 404)
    }
    val jsonStreamFile1 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJson.json"))
    val jsonMeta1 = try { Json.parse(jsonStreamFile1) } finally { jsonStreamFile1.close() }
    val met1 = decode[MetaCatalog](jsonMeta1.toString())
    met1 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.getCause}");
      case Right(metacatalog) => logger.debug("Success decode"); testMongoDB.addMetaCatalog(metacatalog)
    }
    val testNotFound = testMongoDB.getPublicMetaCatalogByName("bandi_test_consip")
    testNotFound map { x =>
      logger.debug("\ndef getPublicMetaCatalogByName\nFile json = /test/FileJson/TestJson.json\nParameters: bandi_test_consip\nI aspected 404 and the return is:\n" + x)
      assertTrue("Not found(Privatex = true)", x.left.get.code.get == 404)
    }
    val jsonStreamFile2 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJsonPrivatexFalse.json"))
    val jsonMeta2 = try { Json.parse(jsonStreamFile2) } finally { jsonStreamFile2.close() }
    val met2 = decode[MetaCatalog](jsonMeta2.toString())
    met2 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.getCause}");
      case Right(metacatalog) => logger.debug("Success decode"); testMongoDB.addMetaCatalog(metacatalog)
    }
    val testFull = testMongoDB.getPublicMetaCatalogByName("bandi_test_consip_public")
    testFull map { x =>
      logger.debug("\ndef getPublicMetaCatalogByName\nFile json = /test/FileJson/TestJsonPrivatexFalse.json\nParameters: bandi_test_consip_public\nI aspected Metacatalog and the return is:\n" + x)
      assertTrue("Found (Public)", x.isRight)
    }
  }

  test("def getPrivateMetaCatalogByName"){
    val groups: List[String] = List("groupname","group2")
    val fakeGroups: List[String] = List("Fake Group","Group Fake")
    val testEmpty = testMongoDB.getPrivateMetaCatalogByName("bandi_test_consip_public","luca_test",groups)
    testEmpty onComplete { x =>
      logger.debug("\ndef getPrivateMetaCatalogByName\n\nDatabase is empty\nI aspected 404 and the return is:\n" + x)
      assertTrue("Not found", x.get.left.get.code.get == 404)
    }
    val jsonStreamFile1 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJsonPrivatexFalse.json"))
    val jsonMeta1 = try { Json.parse(jsonStreamFile1) } finally { jsonStreamFile1.close() }
    val met1 = decode[MetaCatalog](jsonMeta1.toString())
    met1 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.getCause}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => testMongoDB.addMetaCatalog(metacatalog)
    }
    val testPublic = testMongoDB.getPrivateMetaCatalogByName("bandi_test_consip_public","Fake User",fakeGroups)
    testPublic onComplete { x =>
      logger.debug("\ndef getPrivateMetaCatalogByName\nFile json = TestJson.jsonPrivatexFalse\nparameters: luca_test, acl and privatex = false\nI aspected Metacatalog and the return is:\n" + x)
      assertTrue("Found (not Private)", x.get.isRight)
    }
    val jsonStreamFile2 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJson.json"))
    val jsonMeta2 = try { Json.parse(jsonStreamFile2) } finally { jsonStreamFile2.close() }
    val met2 = decode[MetaCatalog](jsonMeta2.toString())
    met2 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.getCause}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => logger.debug("Success decode"); testMongoDB.addMetaCatalog(metacatalog)
    }
    val testNoUserGroup= testMongoDB.getPrivateMetaCatalogByName("bandi_test_consip","Fake User",fakeGroups)
    testNoUserGroup onComplete { x =>
      logger.debug("\ndef getPrivateMetaCatalogByName\nFile json = TestJson.json\nparameters: Fake User, Fake acl\nI aspected 404 and the return is:\n" + x)
      assertTrue("Not Found (by fake user and fake group)", x.get.left.get.code.get == 404)
    }
    val testUserOk = testMongoDB.getPrivateMetaCatalogByName("bandi_test_consip","luca_test",fakeGroups)
    testUserOk onComplete { x =>
      logger.debug("\ndef getPrivateMetaCatalogByName\nFile json = TestJson.json\nparameters: luca_test, Fake acl\nI aspected Metacatalog and the return is:\n" + x)
      assertTrue("Found (by user)", x.get.right.get.equals(met2.right.get))
    }
    val testGroupOk = testMongoDB.getPrivateMetaCatalogByName("bandi_test_consip","Fake User",groups)
    testGroupOk onComplete { x =>
      logger.debug("\ndef getPrivateMetaCatalogByName\nFile json = TestJson.json\nparameters: Fake User, acl\nI aspected Metacatalog and the return is:\n" + x)
      assertTrue("Found (by group)", x.get.right.get.equals(met2.right.get))
    }
    val testUserGroupOK = testMongoDB.getPrivateMetaCatalogByName("bandi_test_consip","luca_test",groups)
    testUserGroupOK onComplete { x =>
      logger.debug("\ndef getPrivateMetaCatalogByName\nFile json = TestJson.json\nparameters: luca_test, acl\nI aspected Metacatalog and the return is:\n" + x)
      assertTrue("Found (by user and group)", x.get.right.get.equals(met2.right.get))
    }
  }

  test("def getMetaCatalogByLogicalUri"){
    val fakeTest = "daf://Fake/Test"
    val test = "daf://dataset/pac_consip/GOVE__amministrazione/bandi_test_consip"
    val testEmpty = testMongoDB.getMetaCatalogByLogicalUri(test)
    logger.debug("\ndef getMetaCatalogByLogicalUri\n\nDatabase is empty\nI aspected 404 and the return is:\n" + testEmpty)
    assertTrue("Not found(Empty)",testEmpty.isLeft)
    val jsonStreamFile = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJson.json"))
    val jsonMeta = try { Json.parse(jsonStreamFile) } finally { jsonStreamFile.close() }
    val met = decode[MetaCatalog](jsonMeta.toString())
    met match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => logger.debug("Success decode"); testMongoDB.addMetaCatalog(metacatalog)
    }
    val testNotFound = testMongoDB.getMetaCatalogByLogicalUri(fakeTest)
    logger.debug("\ndef getMetaCatalogByLogicalUri\n\nDatabase is empty\nI aspected 404 and the return is:\n" + testNotFound)
    assertTrue("Not found(Empty)",testNotFound.isLeft)
    val testFull = testMongoDB.getMetaCatalogByLogicalUri(test)
    logger.debug("\ndef getMetaCatalogByLogicalUri\nFile json=TestJson.json\n\nI aspected Metacatalog and the return is:\n" + testFull)
    assertTrue("Found(by LogicalUri)",testFull.isRight)
  }

  test("def getDatasetStandardFields"){
    val groups: List[String] = List("groupname","group2")
    val groups2: List[String] = List("groupname2","group2")
    val fakeGroups: List[String] = List("Fake Group","Group Fake")
    val testEmpty = testMongoDB.getDatasetStandardFields("luca_test",groups)
    testEmpty onComplete { x =>
      logger.debug("\ndef getDatasetStandardFields\n\nDatabase is empty\nI aspected 404 and the return is:\n" + x)
      assertTrue("Not found (empty)", x.get.left.get.code.get == 404)
    }
    val jsonStreamFile1 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJsonIsStdFalse.json"))
    val jsonMeta1 = try { Json.parse(jsonStreamFile1) } finally { jsonStreamFile1.close() }
    val met1 = decode[MetaCatalog](jsonMeta1.toString())
    met1 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => logger.debug("Success decode"); testMongoDB.addMetaCatalog(metacatalog)
    }
    val seqMetacatalog1 = Seq(met1.right.get)
    val datasetNameFields1 = {
        seqMetacatalog1.map{ catalog =>
        DatasetNameFields(catalog.dcatapit.name, catalog.dataschema.avro.fields.get.map(f => f.name))
      }
    }
    val testIsStdFalse = testMongoDB.getDatasetStandardFields("luca_test2",groups2)
    testIsStdFalse onComplete { x =>
      logger.debug("\ndef getDatasetStandardFields\nTestJsonIsStdFalse\nis_std=false\nI aspected 404 and the return is:\n" + x)
      assertTrue("NotFound (IsStd=false)", x.get.left.get.code.get == 404)
    }
    val jsonStreamFile2 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJsonIsStdTrue.json"))
    val jsonMeta2 = try { Json.parse(jsonStreamFile2) } finally { jsonStreamFile2.close() }
    val met2 = decode[MetaCatalog](jsonMeta2.toString())
    met2 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => logger.debug("Success decode"); testMongoDB.addMetaCatalog(metacatalog)
    }
    val seqMetacatalog2 = Seq(met2.right.get)
    val datasetNameFields2 = {
      seqMetacatalog2.map{ catalog =>
        DatasetNameFields(catalog.dcatapit.name, catalog.dataschema.avro.fields.get.map(f => f.name))
      }
    }
    val testFull = testMongoDB.getDatasetStandardFields("luca_test",groups)
    testFull onComplete { x =>
      logger.debug("\ndef getDatasetStandardFields\nFile Json=TestJsonIsStdTrue.json\nParameters: luca_test, acl\nI aspected Seq[DataSetNameFields] and the return is:\n" + x)
      assertTrue("Found by User and Group", x.get.right.get.equals(datasetNameFields2))
    }
    val testUserOk = testMongoDB.getDatasetStandardFields("luca_test",fakeGroups)
    testUserOk onComplete { x =>
      logger.debug("\ndef getDatasetStandardFields\nFile Json=TestJsonIsStdTrue.json\nParameters: luca_test, Fake acl\nI aspected Seq[DataSetNameFields] and the return is:\n" + x)
      assertTrue("Found by User", x.get.right.get.equals(datasetNameFields2))
    }
    val testGroupOk = testMongoDB.getDatasetStandardFields("fake_user",groups)
    testGroupOk onComplete { x =>
      logger.debug("\ndef getDatasetStandardFields\nFile Json=TestJsonIsStdTrue.json\nParameters: Fake User, acl\nI aspected Seq[DataSetNameFields] and the return is:\n" + x)
      assertTrue("Found by Group", x.get.right.get.equals(datasetNameFields2))
    }
    val testNotGroupNotUser = testMongoDB.getDatasetStandardFields("fake_user",fakeGroups)
    testNotGroupNotUser onComplete { x =>
      logger.debug("\ndef getDatasetStandardFields\nFile Json=TestJsonIsStdTrue.json\nParameters: Fake User, Fake acl\nI aspected 404 and the return is:\n" + x)
      assertTrue("Not Found (Fake User and Fake Group)", x.get.left.get.code.get == 404)
    }
  }

  test("def getFieldsVoc"){
    val testEmpty = testMongoDB.getFieldsVoc
    testEmpty onComplete { x =>
      logger.debug("\ndef getFieldsVoc\n\nDatabase is empty\nI aspected 404 and the return is:\n" + x)
      assertTrue("Not found", x.get.left.get.code.get == 404)
    }
    val jsonStreamFile1 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJson.json"))
    val jsonMeta1 = try { Json.parse(jsonStreamFile1) } finally { jsonStreamFile1.close() }
    val met1 = decode[MetaCatalog](jsonMeta1.toString())
    met1 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => logger.debug("Success decode"); testMongoDB.addMetaCatalog(metacatalog)
    }
    val seqMetacatalog1 = Seq(met1.right.get)
    val datasetNameFields1 = {
      seqMetacatalog1.map{ catalog =>
        DatasetNameFields(catalog.dcatapit.name, catalog.dataschema.avro.fields.get.map(f => f.name))}
    }
    val testIsNotVocab = testMongoDB.getFieldsVoc
    testIsNotVocab onComplete { x =>
      logger.debug("\ndef getFieldsVoc\nFile Json=TestJson.json\nis_vocabulary = false\nI aspected 404 and the return is:\n" + x)
      assertTrue("Not Found (is_vocabulary = false)", x.get.left.get.code.get == 404)
    }
    val jsonStreamFile2 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJsonPrivateAndVocabularyTrue.json"))
    val jsonMeta2 = try { Json.parse(jsonStreamFile2) } finally { jsonStreamFile2.close() }
    val met2 = decode[MetaCatalog](jsonMeta2.toString())
    met2 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => logger.debug("Success decode"); testMongoDB.addMetaCatalog(metacatalog)
    }
    val seqMetacatalog2 = Seq(met2.right.get)
    val datasetNameFields2 = {
      seqMetacatalog2.map{ catalog =>
        DatasetNameFields(catalog.dcatapit.name, catalog.dataschema.avro.fields.get.map(f => f.name))}
    }
    val testIsVocab = testMongoDB.getFieldsVoc
    testIsVocab onComplete { x =>
      logger.debug("\ndef getFieldsVoc\nFile Json=TestJsonPrivateAndVocabularyTrue.json\nis_vocabulary = true\nI aspected Seq[DataSetNameFields] and the return is:\n" + x)
      assertTrue("Found (is_vocabulary = true)", x.get.right.get.equals(datasetNameFields2))
    }
  }

  test("def addMetaCatalog"){
    val jsonStreamFile1 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJson.json"))
    val jsonMeta1 = try { Json.parse(jsonStreamFile1) } finally { jsonStreamFile1.close() }
    val met1 = decode[MetaCatalog](jsonMeta1.toString())
    met1 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => {
        logger.debug("Success decode")
        val testStdSchemaNull = testMongoDB.addMetaCatalog(metacatalog)
        testStdSchemaNull map { x =>
          logger.debug("\ndef addMetaCatalog\nFile Json=TestJson.json\nstd_schema=null\nI aspected DafSuccess and the return is:\n" + x)
          assertTrue("add success (std_schema = null)", x.isRight)
        }
      }
    }
    val jsonStreamFile2 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJsonStdSchemaEqualsOtherLogacalUri.json"))
    val jsonMeta2 = try { Json.parse(jsonStreamFile2) } finally { jsonStreamFile2.close() }
    val met2 = decode[MetaCatalog](jsonMeta2.toString())
    met2 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => {
        logger.debug("Success decode")
        val testEqualsLogical_uri = testMongoDB.addMetaCatalog(metacatalog)
        testEqualsLogical_uri map { x =>
          logger.debug("\ndef addMetaCatalog\nFile Json=TestJsonStdSchemaEqualsOtherLogacalUri.json\nstd_schema is equal an other logical_uri\nI aspected DafSuccess and the return is:\n" + x)
          assertTrue("add success (std_schema equals to an other logical_uri)", x.isRight)
        }
      }
    }
    val jsonStreamFile3 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJsonStdSchemaNotEqualsOtherLogacalUri.json"))
    val jsonMeta3 = try { Json.parse(jsonStreamFile3) } finally { jsonStreamFile3.close() }
    val met3 = decode[MetaCatalog](jsonMeta3.toString())
    met3 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => {
        logger.debug("Success decode")
        val testNotEqualsLogical_uri = testMongoDB.addMetaCatalog(metacatalog)
        testNotEqualsLogical_uri map { x =>
          logger.debug("\ndef addMetaCatalog\nFile Json=TestJsonStdSchemaNotEqualsOtherLogacalUri.json\nstd_schema is not equal an other logical_uri\nI aspected 404 and the return is:\n" + x)
          assertTrue("add fail (std_schema is not equals to an other logical_uri)", x.left.get.code == 404)
        }
      }
    }
  }

  test("def internalCatalogByName"){
    val mockws = mock(classOf[WSClient])
    val fakeUser = "fake_user"
    val jsonStreamFile1 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJson.json"))
    val jsonMeta1 = try { Json.parse(jsonStreamFile1) } finally { jsonStreamFile1.close() }
    val met1 = decode[MetaCatalog](jsonMeta1.toString())
    met1 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) =>{
        logger.debug("Success decode")
        testMongoDB.addMetaCatalog(metacatalog)
        val adminAcl = testMongoDB.internalCatalogByName(metacatalog.dcatapit.name, fakeUser, metacatalog.dcatapit.owner_org.get,true,"token",mockws)
        adminAcl onComplete { x =>
          logger.debug("\ndef internalCatalogByName\nFile Json=TestJson.json\nParameters: Fake User, Admin = true, Acl is not empty\nI aspected 404 and the output is:\n" + x)
          assertTrue("404 : admin True and acl is not empty", x.get.left.get.code.get == 404)
        }
        val usercAcl = testMongoDB.internalCatalogByName(metacatalog.dcatapit.name, metacatalog.dcatapit.author.get, metacatalog.dcatapit.owner_org.get,false,"token",mockws)
        usercAcl onComplete { x =>
          logger.debug("\ndef internalCatalogByName\nFile Json=TestJson.json\nParameters: luca_test, Admin = false, Acl is not empty\nI aspected 404 and the output is:\n" + x)
          assertTrue("404 : luca_test and acl is not empty", x.get.left.get.code.get == 404)
        }
      }
    }
    val jsonStreamFile2 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJsonPrivatexFalse.json"))
    val jsonMeta2 = try { Json.parse(jsonStreamFile2) } finally { jsonStreamFile2.close() }
    val met2 = decode[MetaCatalog](jsonMeta2.toString())
    met2 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => {
        logger.debug("Success decode")
        testMongoDB.addMetaCatalog(metacatalog)
        val adminNoAcl = testMongoDB.internalCatalogByName(metacatalog.dcatapit.name, fakeUser, metacatalog.dcatapit.owner_org.get,true,"token",mockws)
        adminNoAcl map { x =>
          logger.debug("\ndef internalCatalogByName\nFile Json=TestJsonPrivatexFalse.json\nParameters: Fake User, Admin = true, Acl is empty\nI aspected a Metacatalog and the output is:\n" + x)
          assertTrue("Admin and Acl empty", x.isRight)
        }
        val userNoAcl = testMongoDB.internalCatalogByName(metacatalog.dcatapit.name, metacatalog.dcatapit.author.get, metacatalog.dcatapit.owner_org.get,false,"token",mockws)
        userNoAcl map { x =>
          logger.debug("\ndef internalCatalogByName\nFile Json=TestJsonPrivatexFalse.json\nParameters: luca_test, Admin = false, Acl is empty\nI aspected a Metacatalog and the output is:\n" + x)
          assertTrue("luca_test and Acl empty", x.isRight)
        }
        val notAdminNotUserNoAcl = testMongoDB.internalCatalogByName(metacatalog.dcatapit.name, fakeUser, metacatalog.dcatapit.owner_org.get,false,"token",mockws)
        notAdminNotUserNoAcl map { x =>
          logger.debug("\ndef internalCatalogByName\nFile Json=TestJsonPrivatexFalse.json\nParameters: Fake User, Admin = false, Acl is empty\nI aspected 404 and the output is:\n" + x)
          assertTrue("admin false fake_user and Acl empty", x.left.get.code.get == 404)
        }
        val canDeleteCatalogFalse = testMongoDB.internalCatalogByName(metacatalog.dcatapit.name, fakeUser, metacatalog.dcatapit.owner_org.get,true,"fake_token",mockws)
        canDeleteCatalogFalse map { x =>
          logger.debug("\ndef internalCatalogByName\nFile Json=TestJsonPrivatexFalse.json\nParameters: fake User, Admin = true, Acl is empty, token = 'fake_token'(canDeleteCatalog return false)\nI aspected 403 and the output is:\n" + x)
          assertTrue("403: admin true, canDeleteCatalog return false and Acl is empty", x.left.get.code.get == 403)
        }
      }
    }
  }

  test("def deleteCatalogByName"){
    val mockws = mock(classOf[WSClient])
    val fakeUser = "fake_user"
    val jsonStreamFile1 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJson.json"))
    val jsonMeta1 = try { Json.parse(jsonStreamFile1) } finally { jsonStreamFile1.close() }
    val met1 = decode[MetaCatalog](jsonMeta1.toString())
    met1 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) =>{
        logger.debug("Success decode")
        testMongoDB.addMetaCatalog(metacatalog)
        val adminAcl = testMongoDB.deleteCatalogByName(metacatalog.dcatapit.name, fakeUser, metacatalog.dcatapit.owner_org.get,true,"token",mockws)
        adminAcl onComplete { x =>
          logger.debug("\ndef deleteCatalogByName\nFile Json=TestJson.json\nParameters: Fake User, Admin = true, Acl is not empty\nI aspected 404 and the output is:\n" + x)
          assertTrue("404 : admin True and acl is not empty", x.get.left.get.code.get == 404)
        }
        testMongoDB.addMetaCatalog(metacatalog)
        val usercAcl = testMongoDB.deleteCatalogByName(metacatalog.dcatapit.name, metacatalog.dcatapit.author.get, metacatalog.dcatapit.owner_org.get,false,"token",mockws)
        usercAcl onComplete { x =>
          logger.debug("\ndef deleteCatalogByName\nFile Json=TestJson.json\nParameters: luca_test, Admin = false, Acl is not empty\nI aspected 404 and the output is:\n" + x)
          assertTrue("404 : luca_test and acl is not empty", x.get.left.get.code.get == 404)
        }
      }
    }
    val jsonStreamFile2 = new FileInputStream(Environment.simple().getFile("/test/FileJson/TestJsonPrivatexFalse.json"))
    val jsonMeta2 = try { Json.parse(jsonStreamFile2) } finally { jsonStreamFile2.close() }
    val met2 = decode[MetaCatalog](jsonMeta2.toString())
    met2 match {
      case Left(error)        => logger.debug(s"error in parsing response from mongo: ${error.toString}"); Left(Error(Some(500), error.getMessage, None))
      case Right(metacatalog) => {
        logger.debug("Success decode")
        testMongoDB.addMetaCatalog(metacatalog)
        val adminNoAcl = testMongoDB.deleteCatalogByName(metacatalog.dcatapit.name, fakeUser, metacatalog.dcatapit.owner_org.get,true,"token",mockws)
        adminNoAcl map { x =>
          logger.debug("\ndef deleteCatalogByName\nFile Json=TestJsonPrivatexFalse.json\nParameters: Fake User, Admin = true, Acl is empty\nI aspected a Metacatalog and the output is:\n" + x)
          assertTrue("Admin and Acl empty", x.isRight)
        }
        testMongoDB.addMetaCatalog(metacatalog)
        val userNoAcl = testMongoDB.deleteCatalogByName(metacatalog.dcatapit.name, metacatalog.dcatapit.author.get, metacatalog.dcatapit.owner_org.get,false,"token",mockws)
        userNoAcl map { x =>
          logger.debug("\ndef deleteCatalogByName\nFile Json=TestJsonPrivatexFalse.json\nParameters: luca_test, Admin = false, Acl is empty\nI aspected a Metacatalog and the output is:\n" + x)
          assertTrue("luca_test and Acl empty", x.isRight)
        }
        testMongoDB.addMetaCatalog(metacatalog)
        val notAdminNotUserNoAcl = testMongoDB.deleteCatalogByName(metacatalog.dcatapit.name, fakeUser, metacatalog.dcatapit.owner_org.get,false,"token",mockws)
        notAdminNotUserNoAcl map { x =>
          logger.debug("\ndef deleteCatalogByName\nFile Json=TestJson.json\nParameters: fake User, Admin = false, Acl is empty\nI aspected 404 and the output is:\n" + x)
          assertTrue("admin false fake_user and Acl empty", x.left.get.code.get == 404)
        }
        testMongoDB.addMetaCatalog(metacatalog)
        val canDeleteCatalogFalse = testMongoDB.deleteCatalogByName(metacatalog.dcatapit.name, fakeUser, metacatalog.dcatapit.owner_org.get,true,"fake_token",mockws)
        canDeleteCatalogFalse map { x =>
          logger.debug("\ndef deleteCatalogByName\nFile Json=TestJsonPrivatexFalse.json\nParameters: fake User, Admin = true, Acl is empty, token = 'fake_token'(canDeleteCatalog return false)\nI aspected 500 and the output is:\n" + x)
          assertTrue("500: admin true, canDeleteCatalog return false and Acl is empty", x.left.get.code.get == 500)
        }
      }
    }
  }
}
