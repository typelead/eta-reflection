{-# LANGUAGE DataKinds, TypeFamilies #-}
module Java.Reflection.Types where

import Java
import Java.Array
import Java Exception
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

-- Start java.lang.reflect.Field

data Field = Field (@java.lang.reflect.Field)
  deriving Class

type instance Inherits Field = '[AccessibleObject, Object]

foreign import java unsafe getBoolean :: Object -> Java Field Bool

foreign import java unsafe getByte :: Object -> Java Field Byte

foreign import java unsafe getChar :: Object -> Java Field JChar

foreign import java unsafe getDouble :: Object -> Java Field Double

foreign import java unsafe getFloat :: Object -> Java Field Float

foreign import java unsafe getInt :: Object -> Java Field Int

foreign import java unsafe getLong :: Object -> Java Field Long

foreign import java unsafe getShort :: Object -> Java Field Short

foreign import java unsafe isEnumConstant :: Java Field Bool

foreign import java unsafe "isSynthetic" isSyntheticField :: Java Field Bool

foreign import java unsafe set :: Object -> Object -> Java Field ()

foreign import java unsafe setBoolean :: Object -> Bool -> Java Field ()

foreign import java unsafe setByte :: Object -> Byte -> Java Field ()

foreign import java unsafe setChar :: Object -> JChar -> Java Field ()

foreign import java unsafe setDouble :: Object -> Double -> Java Field ()

foreign import java unsafe setFloat :: Object -> Float -> Java Field ()

foreign import java unsafe setInt :: Object -> Int -> Java Field ()

foreign import java unsafe setLong :: Object -> Long -> Java Field ()

foreign import java unsafe setShort :: Object -> Short -> Java Field ()

-- End java.lang.reflect.Field

-- Start java.lang.reflect.Method

data Method = Method (@java.lang.reflect.Method)
  deriving Class

type instance Inherits Method = '[AccessibleObject, Object]

-- End java.lang.reflect.Method

-- Start java.lang.reflect.Modifier

data Modifier = Modifier (@java.lang.reflect.Modifier)
  deriving Class

foreign import java unsafe "@static @field java.lang.Modifier.ABSTRACT" modABSTRACT :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.FINAL" modFINAL :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.INTERFACE" modINTERFACE :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.NATIVE" modNATIVE :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.PRIVATE" modPRIVATE :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.PROTECTED" modPROTECTED :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.PUBLIC" modPUBLIC :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.STATIC" modSTATIC :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.STRICT" modSTRICT :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.SYNCHRONIZED" modSYNCHRONIZED :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.TRANSIENT" modTRANSIENT :: Modifier

foreign import java unsafe "@static @field java.lang.Modifier.VOLATILE" modVOLATILE :: Modifier


-- End java.lang.reflect.Modifier

-- Start java.lang.reflect.Proxy

data Proxy = Proxy (@java.lang.reflect.Proxy)
  deriving Class

-- End java.lang.reflect.Proxy

-- Start java.lang.reflect.ReflectPermission

data ReflectPermission = ReflectPermission (@java.lang.reflect.ReflectPermission)
  deriving Class

type instance Inherits ReflectPermission = '[BasicPermission, Permission]

-- End java.lang.reflect.ReflectPermission

-- Start java.lang.reflect.AnnotatedElement

data AnnotatedElement = AnnotatedElement (@java.lang.reflect.AnnotatedElement)
  deriving Class

foreign import java unsafe "@interface getAnnotation" getAnnotationAE :: (t <: Annotation)
    => JClass t -> Java AnnotatedElement t

foreign import java unsafe "@interface getAnnotations" getAnnotationsAE :: Java AnnotatedElement AnnotationArray

foreign import java unsafe "@interface getDeclaredAnnotations" getDeclaredAnnotationsAE :: Java AnnotatedElement AnnotationArray

foreign import java unsafe "@interface isAnnotationPresent" isAnnotationPresentAE :: (t <: Annotation)
    => JClass t -> Java AnnotatedElement Bool

-- End java.lang.reflect.Proxy

-- Start java.lang.reflect.GenericArrayType

data GenericArrayType = GenericArrayType (@java.lang.reflect.GenericArrayType)
  deriving Class

foreign import java unsafe "@interface getGenericComponentType" getGenericComponentType ::
    Java GenericArrayType Type

-- End java.lang.reflect.GenericArrayType

-- Start java.lang.reflect.GenericDeclaration

data GenericDeclaration = GenericDeclaration (@java.lang.reflect.GenericDeclaration)
  deriving Class

-- End java.lang.reflect.GenericDeclaration

-- Start java.lang.reflect.InvocationHandler

data InvocationHandler = InvocationHandler (@java.lang.reflect.InvocationHandler)
  deriving Class

-- End java.lang.reflect.InvocationHandler

-- Start java.lang.reflect.Member

data Member = Member (@java.lang.reflect.Member)
  deriving Class

-- End java.lang.reflect.Member

-- Start java.lang.reflect.ParameterizedType

data ParameterizedType = ParameterizedType (@java.lang.reflect.ParameterizedType)
  deriving Class

-- End java.lang.reflect.ParameterizedType

-- Start java.lang.reflect.TypeVariable

data TypeVariable d = TypeVariable (@java.lang.reflect.TypeVariable d)
  deriving Class

-- End java.lang.reflect.ParameterizedType

-- Start java.lang.reflect.WildcarType

data WildcardType = WildcardType (@java.lang.reflect.WildcardType)
  deriving Class

-- End java.lang.reflect.WildcardType

-- Start java.lang.reflect.InvocationTargetException

data InvocationTargetException = InvocationTargetException (@java.lang.reflect.InvocationTargetException)
  deriving (Class, Typeable)

type instance Inherits InvocationTargetException = '[ReflectiveOperationException, Exception]

-- End java.lang.reflect.InvocationTargetException

-- Start java.lang.reflect.MalformedParameterizedTypeException

data MalformedParameterizedTypeException = MalformedParameterizedTypeException (@java.lang.reflect.MalformedParameterizedTypeException)
  deriving (Class, Typeable)

type instance Inherits MalformedParameterizedTypeException = '[RuntimeException, Exception]

-- End java.lang.reflect.MalformedParameterizedTypeException

-- Start java.lang.reflect.UndeclaredThrowableException

data UndeclaredThrowableException = UndeclaredThrowableException (@java.lang.reflect.UndeclaredThrowableException)
  deriving (Class, Typeable)

type instance Inherits UndeclaredThrowableException = '[RuntimeException, Exception]

-- End java.lang.reflect.UndeclaredThrowableException
