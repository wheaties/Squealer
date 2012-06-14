package com.wheaties.squealer.sql.validate

import org.specs2.mutable.Specification
import seekwell._
import com.wheaties.squealer.sql.{Success, Failure}
import com.wheaties.squealer.db.{BlobType, DataType, PrimaryKey, Column => DBColumn, Table => DBTable, IntType}

class ValidateUnaryConditionSpec extends Specification{
  "parseExpression" should{
    def cond(expr: Expression) = UnaryCondition(expr, IsNull)

    "fail an aliased expression" in{
      val expr = cond(Aliased(Column(None, "foo"), "bar"))
      val valid = new ValidateUnaryCondition(expr)(Nil)
      valid must beAnInstanceOf[Failure[Exception,Condition]]
    }
    "fail a wildcard" in{
      val expr = cond(Wildcard(None))
      val valid = new ValidateUnaryCondition(expr)(Nil)
      valid must beAnInstanceOf[Failure[Exception,Condition]]
    }
    "fail a function" in{
      val expr = cond(Function(Nil, "foo"))
      val valid = new ValidateUnaryCondition(expr)(Nil)
      valid must beAnInstanceOf[Failure[Exception,Condition]]
    }
    "fail a negate" in{
      val expr = cond(Negate(Column(None, "foo")))
      val valid = new ValidateUnaryCondition(expr)(Nil)
      valid must beAnInstanceOf[Failure[Exception,Condition]]
    }
    "fail a binary operation" in{
      val expr = cond(Add(Column(None, "foo"), Column(None, "foo")))
      val valid = new ValidateUnaryCondition(expr)(Nil)
      valid must beAnInstanceOf[Failure[Exception,Condition]]
    }
    "fail a bind param" in{
      val expr = cond(BindParam("foo", false))
      val valid = new ValidateUnaryCondition(expr)(Nil)
      valid must beAnInstanceOf[Failure[Exception,Condition]]
    }
    "fail a subselect" in{
      val expr = cond(Subselect(SelectStatement(true, Nil, Nil, Nil, None, None, Nil)))
      val valid = new ValidateUnaryCondition(expr)(Nil)
      valid must beAnInstanceOf[Failure[Exception,Condition]]
    }
    "fail a column that isn't in the tables list" in{
      val column = DBColumn("oh noes!", IntType, None, None, PrimaryKey)
      val tables = DBTable("foo", None, column :: Nil) :: Nil
      val expr = cond(Column(None, "bar"))
      val valid = new ValidateUnaryCondition(expr)(tables)
      valid must beAnInstanceOf[Failure[Exception,Condition]]
    }
    "pass a column that's in the tables list" in{
      val column = DBColumn("bar", IntType, None, None, PrimaryKey)
      val tables = DBTable("foo", None, column :: Nil) :: Nil
      val expr = cond(Column(None, "bar"))
      val valid = new ValidateUnaryCondition(expr)(tables)
      valid must be_==(Success(expr))
    }
  }
}

class ValidateComparisonConditionSpec extends Specification{
  "validateColType" should{
    def cond(operator: BinaryOperator) = ComparisonCondition(Column(None, "foo"), operator, StringValue("foo"))
    "pass equals regardless of column type" in{
      val expr = cond(Equals)
      val valid = new ValidateComparisonCondition(expr)
      valid.validateColType(DBColumn("foo", IntType, None, None, PrimaryKey)) must be_==(Success(IntType))
    }
    "pass not equals regardless of column type" in{
      val expr = cond(NotEquals)
      val valid = new ValidateComparisonCondition(expr)
      valid.validateColType(DBColumn("foo", IntType, None, None, PrimaryKey)) must be_==(Success(IntType))
    }
    "pass if numeric" in{
      val expr = cond(LessThan)
      val valid = new ValidateComparisonCondition(expr)
      valid.validateColType(DBColumn("foo", IntType, None, None, PrimaryKey)) must be_==(Success(IntType))
    }
    "fail if not numeric" in{
      val expr = cond(LessThan)
      val valid = new ValidateComparisonCondition(expr)
      valid.validateColType(DBColumn("foo", BlobType, None, None, PrimaryKey)) must beAnInstanceOf[Failure[Exception,DataType]]
    }
  }

  "parseExpression" should{
    false must beTrue
  }
}

class ValidateLikeConditionSpec extends Specification{

}