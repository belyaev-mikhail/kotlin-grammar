/**
 * Kotlin Grammar for ANTLR v4
 *
 * Based on:
 * http://jetbrains.github.io/kotlin-spec/#_grammars_and_parsing
 * and
 * http://kotlinlang.org/docs/reference/grammar.html
 *
 * Tested on
 * https://github.com/JetBrains/kotlin/tree/master/compiler/testData/psi
 */

parser grammar KotlinParser;

options { tokenVocab = KotlinLexer; }

kotlinFile
    : shebangLine? NL* fileAnnotation* packageHeader importList topLevelObject* EOF
    ;

script
    : shebangLine? NL* fileAnnotation* packageHeader importList (statement semi)* EOF
    ;

fileAnnotation
    : (FILE COLON (LSQUARE unescapedAnnotation+ RSQUARE | unescapedAnnotation) semi)+
    ;

packageHeader
    : (PACKAGE identifier semi)?
    ;

importList
    : importHeader*
    ;

importHeader
    : IMPORT identifier (DOT MULT | importAlias)? semi
    ;

importAlias
    : AS simpleIdentifier
    ;

topLevelObject
    : (classDeclaration
    | functionDeclaration
    | objectDeclaration
    | propertyDeclaration
    | typeAlias) semis
    ;

classDeclaration
    : modifierList? (CLASS | INTERFACE) NL* simpleIdentifier
    (NL* typeParameters)? (NL* primaryConstructor)?
    (NL* COLON NL* delegationSpecifiers)?
    (NL* typeConstraints)?
    (NL* classBody | NL* enumClassBody)?
    ;

primaryConstructor
    : modifierList? (CONSTRUCTOR NL*)? classParameters
    ;

classParameters
    : LPAREN NL* (classParameter (NL* COMMA NL* classParameter)*)? NL* RPAREN
    ;

classParameter
    : modifierList? (VAL | VAR)? NL* simpleIdentifier COLON NL* type (NL* ASSIGNMENT NL* expression)?
    ;

delegationSpecifiers // wrong: every delegationSpecifier may contain annotations
    : annotatedDelegationSpecifier (NL* COMMA NL* annotatedDelegationSpecifier)*
    ;

annotatedDelegationSpecifier
    : annotations* NL* delegationSpecifier
    ;

delegationSpecifier
    : constructorInvocation
    | userType
    | functionType
    | explicitDelegation
    ;

constructorInvocation
    : userType callSuffix
    ;

explicitDelegation
    : (userType | functionType) NL* BY NL* expression
    ;

classBody
    : LCURL NL* classMemberDeclarations? NL* RCURL
    ;

classMemberDeclarations
    : (classMemberDeclaration semis)* classMemberDeclaration semis?
    ;

classMemberDeclaration
    : classDeclaration
    | functionDeclaration
    | objectDeclaration
    | companionObject
    | propertyDeclaration
    | anonymousInitializer
    | secondaryConstructor
    | typeAlias
    ;

anonymousInitializer
    : INIT NL* block
    ;

secondaryConstructor
    : modifierList? CONSTRUCTOR NL* functionValueParameters (NL* COLON NL* constructorDelegationCall)? NL* block?
    ;

constructorDelegationCall
    : THIS NL* valueArguments
    | SUPER NL* valueArguments
    ;

enumClassBody
    : LCURL NL* enumEntries? (NL* SEMICOLON NL* classMemberDeclarations?)? NL* RCURL
    ;

enumEntries
    : (enumEntry (NL* COMMA NL* enumEntry)* NL* COMMA?)
    ;

enumEntry
    : (modifierList NL*)? simpleIdentifier (NL* valueArguments)? (NL* classBody)?
    ;

functionDeclaration
    : modifierList? FUN
    (NL* typeParameters)?
    (NL* type NL* DOT)?
    (NL* simpleIdentifier)
    NL* functionValueParameters
    (NL* COLON NL* type)?
    (NL* typeConstraints)?
    (NL* functionBody)?
    ;

functionValueParameters
    : LPAREN NL* (functionValueParameter (NL* COMMA NL* functionValueParameter)*)? NL* RPAREN
    ;

functionValueParameter
    : modifierList? parameter (NL* ASSIGNMENT NL* expression)?
    ;

parameter
    : simpleIdentifier NL* (COLON NL* type)?
    ;

functionBody
    : block
    | ASSIGNMENT NL* expression
    ;

objectDeclaration
    : modifierList? OBJECT
    NL* simpleIdentifier
    (NL* COLON NL* delegationSpecifiers)?
    (NL* classBody)?
    ;

companionObject
    : modifierList? COMPANION NL* modifierList? OBJECT
    (NL* simpleIdentifier)?
    (NL* COLON NL* delegationSpecifiers)?
    (NL* classBody)?
    ;

propertyDeclaration
    : modifierList? (VAL | VAR)
    (NL* typeParameters)?
    (NL* type NL* DOT)?
    (NL* (multiVariableDeclaration | variableDeclaration))
    (NL* typeConstraints)?
    (NL* (BY | ASSIGNMENT) NL* expression)?
    (NL+ SEMICOLON)? NL* (getter? (NL* semi? setter)? | setter? (NL* semi? getter)?)
    /*
        XXX: actually, it's not that simple. You can put semi only on the same line as getter, but any other semicolons
        between property and getter are forbidden
        Is this a bug in kotlin parser? Who knows.
    */
    ;

multiVariableDeclaration
    : LPAREN NL* variableDeclaration (NL* COMMA NL* variableDeclaration)* NL* RPAREN
    ;

variableDeclaration
    : annotations* NL* simpleIdentifier (NL* COLON NL* type)?
    ;

getter
    : modifierList? GETTER
    | modifierList? GETTER NL* LPAREN NL* RPAREN (NL* COLON NL* type)? NL* functionBody
    ;

setter
    : modifierList? SETTER
    | modifierList? SETTER NL* LPAREN (annotations | parameterModifier)* parameter RPAREN (NL* COLON NL* type)? NL* functionBody
    ;

typeAlias
    : modifierList? TYPE_ALIAS NL* simpleIdentifier (NL* typeParameters)? NL* ASSIGNMENT NL* type
    ;

typeParameters
    : LANGLE NL* typeParameter (NL* COMMA NL* typeParameter)* NL* RANGLE
    ;

typeParameter
    : modifierList? NL* simpleIdentifier (NL* COLON NL* type)?
    ;

type
    : typeModifierList?
    ( parenthesizedType
    | nullableType
    | typeReference
    | functionType)
    ;

typeModifierList
    : (annotations | SUSPEND NL*)+
    ;

parenthesizedType
    : LPAREN type RPAREN
    ;

nullableType
    : (typeReference | parenthesizedType) NL* QUEST+
    ;

typeReference
    : LPAREN typeReference RPAREN
    | userType
    | DYNAMIC
    ;

functionType
    : (receiverType NL* DOT NL*)? functionTypeParameters  NL* ARROW (NL* type)?
    ;

receiverType
    : parenthesizedType
    | nullableType
    | typeReference
    ;

userType
    : simpleUserType (NL* DOT NL* simpleUserType)*
    ;

simpleUserType
    : simpleIdentifier (NL* typeArguments)?
    ;

//parameters for functionType
functionTypeParameters
    : LPAREN NL* (parameter | type)? (NL* COMMA NL* (parameter | type))* NL* RPAREN
    ;

typeConstraints
    : WHERE NL* typeConstraint (NL* COMMA NL* typeConstraint)*
    ;

typeConstraint
    : annotations* simpleIdentifier NL* COLON NL* type
    ;

block
    : LCURL NL* statements NL* RCURL
    ;

statements
    : (statement (semis statement)* semis?)?
    ;

statement
    : labelDefinition*
    ( declaration
    | assignment
    | loopStatement
    | expression)
    ;

declaration
    : classDeclaration
    | objectDeclaration
    | functionDeclaration
    | propertyDeclaration
    | typeAlias
    ;

assignment
    : directlyAssignableExpression ASSIGNMENT NL* expression
    | assignableExpression assignmentAndOperator NL* expression
    ;

expression
    : disjunction | ifExpression
    ;

ifExpression
    : IF NL* LPAREN NL* expression NL* RPAREN NL* controlStructureBody (SEMICOLON? NL* ELSE NL* controlStructureBody)?
    | IF NL* LPAREN NL* expression NL* RPAREN NL* SEMICOLON? NL* ELSE NL* controlStructureBody
    ;

disjunction
    : conjunction (NL* DISJ NL* (conjunction | ifExpression))*
    ;

conjunction
    : equality (NL* CONJ NL* (equality | ifExpression))*
    ;

equality
    : comparison (/* NO NL! */ equalityOperator NL* (comparison | ifExpression))*
    ;

comparison
    : infixOperation (/* NO NL! */ comparisonOperator NL* (infixOperation | ifExpression))?
    ;

infixOperation
    : elvisExpression (/* NO NL! */ inOperator NL* (elvisExpression | ifExpression) | isOperator NL* type)*
    ;

elvisExpression
    : infixFunctionCall (NL* ELVIS NL* (infixFunctionCall | ifExpression))*
    ;

infixFunctionCall
    : rangeExpression (/* NO NL! */ simpleIdentifier NL* (rangeExpression | ifExpression))*
    ;

rangeExpression
    : additiveExpression (/* NO NL! */ RANGE NL* (additiveExpression | ifExpression))*
    ;

additiveExpression
    : multiplicativeExpression (/* NO NL! */ additiveOperator NL* (multiplicativeExpression | ifExpression))*
    ;

multiplicativeExpression
    : asExpression (/* NO NL! */ multiplicativeOperator NL* (asExpression | ifExpression))*
    ;

asExpression
    : prefixUnaryExpression (NL* asOperator NL* type)?
    ;

prefixUnaryExpression
    : unaryPrefix* postfixUnaryExpression
    | unaryPrefix+ ifExpression
    ;

unaryPrefix
    : annotations
    | IdentifierAt NL*
    | prefixUnaryOperator NL*
    ;

postfixUnaryExpression
    : primaryExpression (postfixUnarySuffix)*
    ;

postfixUnarySuffix
    : postfixUnaryOperator
    | typeArguments
    | callSuffix
    | indexingSuffix
    | navigationSuffix
    ;

directlyAssignableExpression
    : postfixUnaryExpression assignableSuffix
    | simpleIdentifier
    ;

assignableExpression
    : prefixUnaryExpression
    ;

assignableSuffix
    : typeArguments
    | indexingSuffix
    | navigationSuffix
    ;

indexingSuffix
    : LSQUARE NL* (expression NL* (COMMA NL* expression NL*)*) NL* RSQUARE
    ;

navigationSuffix
    : NL* memberAccessOperator NL* (simpleIdentifier | CLASS)
    ;

callSuffix
    : typeArguments? valueArguments? annotatedLambda
    | typeArguments? valueArguments
    ;

annotatedLambda
    : (annotations | IdentifierAt)* NL* lambdaLiteral
    ;

valueArguments
    : LPAREN NL* valueArgument? NL* RPAREN
    | LPAREN NL* valueArgument (NL* COMMA NL* valueArgument)* NL* RPAREN
    ;

typeArguments
    : LANGLE NL* typeProjection (NL* COMMA typeProjection)* NL* RANGLE
    ;

typeProjection
    : typeProjectionModifierList? type | MULT
    ;

typeProjectionModifierList
    : varianceAnnotation+
    ;

valueArgument
    : annotations? NL* (simpleIdentifier NL* ASSIGNMENT NL*)? MULT? NL* expression
    ;

primaryExpression
    : parenthesizedExpression
    | literalConstant
    | stringLiteral
    | simpleIdentifier
    | callableReference
    | functionLiteral
    | objectLiteral
    | collectionLiteral
    | thisExpression
    | superExpression
    | whenExpression
    | tryExpression
    | jumpExpression
    ;

parenthesizedExpression
    : LPAREN NL* expression NL* RPAREN
    ;

collectionLiteral
    : LSQUARE NL* expression (NL* COMMA NL* expression)* NL* RSQUARE
    | LSQUARE NL* RSQUARE
    ;

literalConstant
    : BooleanLiteral
    | IntegerLiteral
    | HexLiteral
    | BinLiteral
    | CharacterLiteral
    | RealLiteral
    | NullLiteral
    | LongLiteral
    ;

stringLiteral
    : lineStringLiteral
    | multiLineStringLiteral
    ;

lineStringLiteral
    : QUOTE_OPEN (lineStringContent | lineStringExpression)* QUOTE_CLOSE
    ;

multiLineStringLiteral // why is lineStringLiteral here? there is no escaping in multiline strings
    : TRIPLE_QUOTE_OPEN (multiLineStringContent | multiLineStringExpression | lineStringLiteral | MultiLineStringQuote)* TRIPLE_QUOTE_CLOSE
    ;

lineStringContent
    : LineStrText
    | LineStrEscapedChar
    | LineStrRef
    ;

lineStringExpression
    : LineStrExprStart expression RCURL
    ;

multiLineStringContent
    : MultiLineStrText
    | MultiLineStringQuote
    | MultiLineStrRef
    ;

multiLineStringExpression
    : MultiLineStrExprStart NL* expression NL* RCURL
    ;

lambdaLiteral // anonymous functions?
    : LCURL NL* statements NL* RCURL
    | LCURL NL* lambdaParameters NL* ARROW NL* statements NL* RCURL
    ;

lambdaParameters
    : lambdaParameter? (NL* COMMA NL* lambdaParameter)*
    ;

lambdaParameter
    : variableDeclaration
    | multiVariableDeclaration (NL* COLON NL* type)?
    ;

anonymousFunction
    : FUN
    (NL* type NL* DOT)?
    NL* functionValueParameters
    (NL* COLON NL* type)?
    (NL* typeConstraints)?
    (NL* functionBody)?
    ;

functionLiteral
    : lambdaLiteral
    | anonymousFunction
    ;

objectLiteral
    : OBJECT NL* COLON NL* delegationSpecifiers (NL* classBody)?
    | OBJECT NL* classBody
    ;

thisExpression
    : THIS AtIdentifier?
    | THIS_AT
    ;

superExpression
    : SUPER (LANGLE NL* type NL* RANGLE)? AtIdentifier?
    | SUPER_AT
    ;

controlStructureBody
    : block
    | statement
    ;

whenExpression
    : WHEN NL* (LPAREN expression RPAREN)? NL* LCURL NL* (whenEntry NL*)* NL* RCURL
    ;

whenEntry
    : whenCondition (NL* COMMA NL* whenCondition)* NL* ARROW NL* controlStructureBody semi?
    | ELSE NL* ARROW NL* controlStructureBody semi?
    ;

whenCondition
    : expression
    | rangeTest
    | typeTest
    ;

rangeTest
    : inOperator NL* expression
    ;

typeTest
    : isOperator NL* type
    ;

tryExpression
    : TRY NL* block (NL* catchBlock)* (NL* finallyBlock)?
    ;

catchBlock
    : CATCH NL* LPAREN annotations* simpleIdentifier COLON userType RPAREN NL* block
    ;

finallyBlock
    : FINALLY NL* block
    ;

loopStatement
    : forStatement
    | whileStatement
    | doWhileStatement
    ;

forStatement
    : FOR NL* LPAREN annotations* (variableDeclaration | multiVariableDeclaration) IN expression RPAREN NL* controlStructureBody?
    ;

whileStatement
    : WHILE NL* LPAREN expression RPAREN NL* controlStructureBody?
    ;

doWhileStatement
    : DO NL* controlStructureBody? NL* WHILE NL* LPAREN expression RPAREN
    ;

jumpExpression
    : THROW NL* expression
    | (RETURN | RETURN_AT) expression?
    | CONTINUE | CONTINUE_AT
    | BREAK | BREAK_AT
    ;

callableReference
    : (receiverType? NL* (COLONCOLON | Q_COLONCOLON) NL* (simpleIdentifier | CLASS))
    ;

assignmentAndOperator
    : ASSIGNMENT
    | ADD_ASSIGNMENT
    | SUB_ASSIGNMENT
    | MULT_ASSIGNMENT
    | DIV_ASSIGNMENT
    | MOD_ASSIGNMENT
    ;

equalityOperator
    : EXCL_EQ
    | EXCL_EQEQ
    | EQEQ
    | EQEQEQ
    ;

comparisonOperator
    : LANGLE
    | RANGLE
    | LE
    | GE
    ;

inOperator
    : IN | NOT_IN
    ;

isOperator
    : IS | NOT_IS
    ;

additiveOperator
    : ADD | SUB
    ;

multiplicativeOperator
    : MULT
    | DIV
    | MOD
    ;

asOperator
    : AS
    | AS_SAFE
    | COLON
    ;

prefixUnaryOperator
    : INCR
    | DECR
    | ADD
    | SUB
    | EXCL
    ;

postfixUnaryOperator
    : INCR | DECR | EXCL EXCL
    ;

memberAccessOperator
    : DOT | QUEST DOT | COLONCOLON
    ;

modifierList
    : (annotations | modifier)+
    ;

modifier
    : (classModifier
    | memberModifier
    | visibilityModifier
    | varianceAnnotation
    | functionModifier
    | propertyModifier
    | inheritanceModifier
    | parameterModifier
    | typeParameterModifier
    | platformModifier) NL*
    ;

classModifier
    : ENUM
    | SEALED
    | ANNOTATION
    | DATA
    | INNER
    ;

memberModifier
    : OVERRIDE
    | LATEINIT
    ;

visibilityModifier
    : PUBLIC
    | PRIVATE
    | INTERNAL
    | PROTECTED
    ;

varianceAnnotation
    : IN | OUT
    ;

functionModifier
    : TAILREC
    | OPERATOR
    | INFIX
    | INLINE
    | EXTERNAL
    | SUSPEND
    ;

propertyModifier
    : CONST
    ;

inheritanceModifier
    : ABSTRACT
    | FINAL
    | OPEN
    ;

parameterModifier
    : VARARG
    | NOINLINE
    | CROSSINLINE
    ;

typeParameterModifier
    : REIFIED
    ;

platformModifier
    : EXPECT
    | ACTUAL
    ;

labelDefinition
    : IdentifierAt NL*
    ;

annotations
    : (annotation | annotationList) NL*
    ;

annotation
    : annotationUseSiteTarget COLON NL* unescapedAnnotation
    | AtIdentifier (NL* DOT simpleIdentifier)* typeArguments? valueArguments?
    ;

annotationList
    : annotationUseSiteTarget COLON LSQUARE unescapedAnnotation+ RSQUARE
    | AT LSQUARE unescapedAnnotation+ RSQUARE
    ;

annotationUseSiteTarget
    : FIELD
    | FILE
    | PROPERTY
    | GET
    | SET
    | RECEIVER
    | PARAM
    | SETPARAM
    | DELEGATE
    ;

unescapedAnnotation
    : identifier typeArguments? valueArguments?
    ;

simpleIdentifier
    : Identifier //soft keywords:
    | ABSTRACT
    | ANNOTATION
    | BY
    | CATCH
    | COMPANION
    | CONSTRUCTOR
    | CROSSINLINE
    | DATA
    | DYNAMIC
    | ENUM
    | EXTERNAL
    | FINAL
    | FINALLY
    | GETTER
    | IMPORT
    | INFIX
    | INIT
    | INLINE
    | INNER
    | INTERNAL
    | LATEINIT
    | NOINLINE
    | OPEN
    | OPERATOR
    | OUT
    | OVERRIDE
    | PRIVATE
    | PROTECTED
    | PUBLIC
    | REIFIED
    | SEALED
    | TAILREC
    | SETTER
    | VARARG
    | WHERE
    | EXPECT
    | ACTUAL
    //strong keywords
    | CONST
    | SUSPEND
    ;

identifier
    : simpleIdentifier (NL* DOT simpleIdentifier)*
    ;

shebangLine
    : ShebangLine
    ;

semi: NL+ | SEMICOLON | SEMICOLON NL+ | EOF;
semis
    : semi ((WS | NL)+ semi)*
    ;
