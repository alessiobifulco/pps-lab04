package it.unibo.pps.tasks.adts

import org.junit.* , org.junit.Assert.* , SchoolModel.*
import it.unibo.pps.u03.extensionmethods.Sequences.Sequence.*


class SchoolModelTest {

  val schoolModel: SchoolModule = BasicSchoolModule

  @Test def testCourses() =
    val emptySchool = schoolModel.emptySchool
    assertEquals(Nil(), emptySchool.courses)

  @Test def testteachers() =
    val emptySchool = schoolModel.emptySchool
    assertEquals(Nil(), emptySchool.teachers)

  @Test def testSetTeacherToCourse() =
    val teacher = schoolModel.teacher("Mirko")
    val course = schoolModel.course("PPS")
    val school = schoolModel.emptySchool
    val updatedSchool = school.setTeacherToCourse(teacher, course)
    assertEquals(Cons("Mirko", Nil()), updatedSchool.teachers)
    assertEquals(Cons("PPS", Nil()), updatedSchool.courses)

  @Test def testCourseOfATeacher() =
    val teacher = schoolModel.teacher("Mirko")
    val course = schoolModel.course("PPS")
    val course2 = schoolModel.course("PPS")
    val school = schoolModel.emptySchool
    val updatedSchool = school.setTeacherToCourse(teacher, course)
    val updatedSchool2 = updatedSchool.setTeacherToCourse(teacher, course2)
    assertEquals(Cons(course, Nil()), updatedSchool2.coursesOfATeacher(teacher))

  @Test def testHasTeacher() =
    val teacher = schoolModel.teacher("Mirko")
    val course = schoolModel.course("PPS")
    val school = schoolModel.emptySchool
    val updatedSchool = school.setTeacherToCourse(teacher, course)
    assertTrue(updatedSchool.hasTeacher("Mirko"))
}

