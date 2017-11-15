{-# LANGUAGE DataKinds, TypeFamilies #-}
module Java.Reflection.Types where

import Java
import Java.Array
import Java.Utils

data AccessibleObject = AccessibleObject (@java.lang.reflect.AccessibleObject)
  deriving Class

-- Start java.lang.reflect.AccessibleObject

foreign import java unsafe getAnnotation :: (t <: Annotation) => JClass t -> Java AccessibleObject t

foreign import java unsafe getAnnotations :: Java AccessibleObject AnnotationArray

foreign import java unsafe getDeclaredAnnotations :: Java AccessibleObject AnnotationArray

foreign import java unsafe isAccessible :: Java AccessibleObject Bool

foreign import java unsafe isAnnotationPresent :: (a <: Annotation)
                                               => JClass a -> Java AccessibleObject Bool

foreign import java unsafe setAccessible :: Bool -> Java AccessibleObject ()

-- End java.lang.reflect.AccessibleObject

data Annotation = Annotation (@java.lang.annotation.Annotation)
  deriving Class

-- Start java.lang.annotation.Annotation

foreign import java unsafe "@interface annotationType" annotationType :: (a <: Annotation)
      => Java Annotation (JClass a)

foreign import java unsafe "@interface equals" equalsAnnotation :: Object -> Java Annotation Bool

-- End java.lang.annotation.Annotation

-- Start java.lang.annotation.AnnotationArray

data AnnotationArray = AnnotationArray (@java.lang.annotation.Annotation[])
  deriving Class

instance JArray Annotation AnnotationArray

-- End java.lang.annotation.AnnotationArray

-- Start java.lang.annotation.AnnotationDoubleArray

data AnnotationDoubleArray = AnnotationDoubleArray (@java.lang.annotation.Annotation[][])
  deriving Class

instance JArray Annotation AnnotationDoubleArray

-- End java.lang.annotation.AnnotationDoubleArray

-- Start java.lang.reflect.Array

data Array = Array (@java.lang.reflect.Array)
  deriving Class

-- End java.lang.reflect.Array

-- Start java.lang.reflect.Constructor

data Constructor t = Constructor (@java.lang.reflect.Constructor t)
  deriving Class

foreign import java unsafe "getAnnotation" getAnnotationConstructor :: (t <: Annotation)
      => JClass t -> Java (Constructor t) t

foreign import java unsafe "getDeclaredAnnotations" getDeclaredAnnotationsConstructor :: (t <: Object)
      => JClass t -> Java (Constructor t) AnnotationArray

foreign import java unsafe "getDeclaringClass" getDeclaringClass :: (t <: Object)
      -> Java (Constructor t) (JClass t)

foreign import java unsafe getGenericExceptionTypes :: (t <: Object) -> Java (Constructor t) TypeArray

foreign import java unsafe getGenericParameterTypes :: (t <: Object) -> Java (Constructor t) TypeArray

foreign import java unsafe getModifiers :: (t <: Object) -> Java (Constructor t) Int

foreign import java unsafe getName :: (t <: Object) -> Java (Constructor t) JString

foreign import java unsafe getParameterAnnotations :: (t <: Object)
      -> Java (Constructor t) AnnotationDoubleArray

foreign import java unsafe isSynthetic :: (t <: Object) -> Java (Constructor t) Bool

foreign import java unsafe isVarArgs :: (t <: Object) -> Java (Constructor t) Bool

foreign import java unsafe toGenericString :: (t <: Object) -> Java (Constructor t) JString

-- End java.lang.reflect.Constructor

-- Start java.lang.reflect.Type

data Type = Type (@java.lang.reflect.Type)
  deriving Class

-- End java.lang.reflect.Type

-- Start java.lang.reflect.TypeArray

data TypeArray = TypeArray (@java.lang.reflect.Type[])
  deriving Class

instance JArray Type TypeArray

-- End java.lang.reflect.TypeArray
