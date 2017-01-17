package org.zsd.cats.transform

import cats.data._
import cats.instances.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformerExample {

  //   https://enear.github.io/2016/04/11/monad-transformers/

  sealed trait Employee {
    val id: String
  }

  final case class EmployeeWithoutDetails(id: String) extends Employee

  final case class EmployeeWithDetails(id: String, name: String, city: String, age: Int) extends Employee

  case class Company(companyName: String, employees: List[EmployeeWithoutDetails])

  trait SyncDBOps {
    protected def getDetails(employeeId: String): Option[EmployeeWithDetails]

    protected def getCompany(companyName: String): Option[Company]

    def getEmployeeAge(employeeId: String, companyName: String): Option[Int] = {
      for {
        company <- getCompany(companyName)
        if company.employees map (_.id) contains employeeId
        details <- getDetails(employeeId)
      } yield details.age
    }
  }

  trait AsyncDBOps {
    protected def getDetails(employeeId: String): Future[Option[EmployeeWithDetails]]

    protected def getCompany(companyName: String): Future[Option[Company]]

    def getEmployeeAge1(employeeId: String, companyName: String)(implicit ec: ExecutionContext): Future[Option[Int]] = {
      for {
        companyOpt: Option[Company] <- getCompany(companyName)
        company: Company = companyOpt.getOrElse(Company("error", List())) // explicit error handling
        if company.employees map (_.id) contains employeeId // makes the future fail
        detailsOpt: Option[EmployeeWithDetails] <- getDetails(employeeId)
      } yield detailsOpt map (_.age)
    }

    def getEmployeeAge2(employeeId: String, companyName: String)(implicit ec: ExecutionContext): Future[Option[Int]] = {
      (for {
        company: Company <- OptionT(getCompany(companyName))
        if company.employees map (_.id) contains employeeId
        details <- OptionT(getDetails(employeeId))
      } yield details.age).value
    }
  }

}
