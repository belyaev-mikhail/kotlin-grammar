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
    : ('@file' ':' ('[' unescapedAnnotation+ ']' | unescapedAnnotation) semi)+
    ;

packageHeader
    : ('package' identifier semi)?
    ;

importList
    : importHeader*
    ;

importHeader
    : 'import' identifier ('.' '*' | importAlias)? semi
    ;

importAlias
    : 'as' simpleIdentifier
    ;

topLevelObject
    : declaration semis
    ;

classDeclaration
    : modifierList? ('class' | 'interface') NL* simpleIdentifier
    (NL* typeParameters)? (NL* primaryConstructor)?
    (NL* ':' NL* delegationSpecifiers)?
    (NL* typeConstraints)?
    (NL* classBody | NL* enumClassBody)?
    ;

primaryConstructor
    : modifierList? ('constructor' NL*)? classParameters
    ;

classParameters
    : '(' NL* (classParameter (NL* ',' NL* classParameter)*)? NL* ')'
    ;

classParameter
    : modifierList? ('val' | 'var')? NL* simpleIdentifier ':' NL* type (NL* '=' NL* expression)?
    ;

delegationSpecifiers
    : annotatedDelegationSpecifier (NL* ',' NL* annotatedDelegationSpecifier)*
    ;

annotatedDelegationSpecifier
    : annotation* NL* delegationSpecifier
    ;

delegationSpecifier
    : constructorInvocation
    | explicitDelegation
    | userType
    | functionType
    ;

constructorInvocation
    : userType valueArguments
    ;

explicitDelegation
    : (userType | functionType) NL* 'by' NL* expression
    ;

classBody
    : '{' NL* classMemberDeclarations? NL* '}'
    ;

classMemberDeclarations
    : (classMemberDeclaration semis)* classMemberDeclaration semis?
    ;

classMemberDeclaration
    : declaration
    | companionObject
    | anonymousInitializer
    | secondaryConstructor
    ;

anonymousInitializer
    : 'init' NL* block
    ;

secondaryConstructor
    : modifierList? 'constructor' NL* functionValueParameters (NL* ':' NL* constructorDelegationCall)? NL* block?
    ;

constructorDelegationCall
    : 'this' NL* valueArguments
    | 'super' NL* valueArguments
    ;

enumClassBody
    : '{' NL* enumEntries? (NL* ';' NL* classMemberDeclarations?)? NL* '}'
    ;

enumEntries
    : enumEntry (NL* ',' NL* enumEntry)* NL* ','?
    ;

enumEntry
    : (modifierList NL*)? simpleIdentifier (NL* valueArguments)? (NL* classBody)?
    ;

functionDeclaration
    : modifierList?
    'fun'
    (NL* typeParameters)?
    (NL* type NL* '.')? // ?. here is a disambiguation in cases where tokens '?' and '.' go after each other
    (NL* simpleIdentifier)
    NL* functionValueParameters
    (NL* ':' NL* type)?
    (NL* typeConstraints)?
    (NL* functionBody)?
    ;

functionValueParameters
    : '(' NL* (functionValueParameter (NL* ',' NL* functionValueParameter)*)? NL* ')'
    ;

functionValueParameter
    : modifierList? parameter (NL* '=' NL* expression)?
    ;

parameter
    : simpleIdentifier NL* (':' NL* type)?
    ;

functionBody
    : block
    | '=' NL* expression
    ;

objectDeclaration
    : modifierList? 'object'
    NL* simpleIdentifier
    (NL* ':' NL* delegationSpecifiers)?
    (NL* classBody)?
    ;

companionObject
    : modifierList? 'companion' NL* 'object'
    (NL* simpleIdentifier)?
    (NL* ':' NL* delegationSpecifiers)?
    (NL* classBody)?
    ;

propertyDeclaration
    : modifierList? ('val' | 'var')
    (NL* typeParameters)?
    (NL* type NL* '.')?
    (NL* (multiVariableDeclaration | variableDeclaration))
    (NL* typeConstraints)?
    (NL* ('=' NL* expression | propertyDelegate))?
    (NL+ ';')? NL* (getter? (NL* semi? setter)? | setter? (NL* semi? getter)?)
    /*
        XXX: actually, it's not that simple. You can put semi only on the same line as getter, but any other semicolons
        between property and getter are forbidden
        Is this a bug in kotlin parser? Who knows.
    */
    ;

multiVariableDeclaration
    : '(' NL* variableDeclaration (NL* ',' NL* variableDeclaration)* NL* ')'
    ;

variableDeclaration
    : annotation* NL* simpleIdentifier (NL* ':' NL* type)?
    ;

propertyDelegate
    : 'by' NL* expression
    ;

getter
    : modifierList? 'get'
    | modifierList? 'get' NL* '(' NL* ')' (NL* ':' NL* type)? NL* functionBody
    ;

setter
    : modifierList? 'set'
    | modifierList? 'set' NL* '(' (annotation | parameterModifier)* parameter ')' (NL* ':' NL* type)? NL* functionBody
    ;

typeAlias
    : modifierList? 'typealias' NL* simpleIdentifier (NL* typeParameters)? NL* '=' NL* type
    ;

typeParameters
    : '<' NL* typeParameter (NL* ',' NL* typeParameter)* NL* '>'
    ;

typeParameter
    : modifierList? NL* simpleIdentifier (NL* ':' NL* type)?
    ;

type
    : typeModifierList?
    ( parenthesizedType
    | nullableType
    | typeReference
    | functionType)
    ;

typeModifierList
    : (annotation | 'suspend' NL*)+
    ;

parenthesizedType
    : '(' type ')'
    ;

nullableType
    : (typeReference | parenthesizedType) NL* '?'+
    ;

typeReference
    : '(' typeReference ')'
    | userType
    | 'dynamic' // do we need a separate dynamic support here?
    ;

functionType
    : (receiverType NL* '.' NL*)? functionTypeParameters  NL* '->' (NL* type)?
    ;

receiverType
    : parenthesizedType
    | nullableType
    | typeReference
    ;

userType
    : simpleUserType (NL* '.' NL* simpleUserType)*
    ;

parenthesizedUserType
    : '(' userType ')'
    | '(' parenthesizedUserType ')'
    ;

simpleUserType
    : simpleIdentifier (NL* typeArguments)?
    ;

// parameters for functionType
functionTypeParameters
    : '(' NL* (parameter | type)? (NL* ',' NL* (parameter | type))* NL* ')'
    ;

typeConstraints
    : 'where' NL* typeConstraint (NL* ',' NL* typeConstraint)*
    ;

typeConstraint
    : annotation* simpleIdentifier NL* ':' NL* type
    ;

block
    : '{' NL* statements NL* '}'
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
    : directlyAssignableExpression '=' NL* expression
    | assignableExpression assignmentAndOperator NL* expression
    ;

expression
    : disjunction
    ;

disjunction
    : conjunction (NL* '||' NL* conjunction)*
    ;

conjunction
    : equality (NL* '&&' NL* equality)*
    ;

equality
    : comparison (/* NO NL! */ equalityOperator NL* comparison)*
    ;

comparison
    : infixOperation (/* NO NL! */ comparisonOperator NL* infixOperation)?
    ;

infixOperation
    : elvisExpression (/* NO NL! */ inOperator NL* elvisExpression | isOperator NL* type)*
    ;

elvisExpression
    : infixFunctionCall (NL* '?:' NL* infixFunctionCall)*
    ;

infixFunctionCall
    : rangeExpression (/* NO NL! */ simpleIdentifier NL* rangeExpression)*
    ;

rangeExpression
    : additiveExpression (/* NO NL! */ '..' NL* additiveExpression)*
    ;

additiveExpression
    : multiplicativeExpression (/* NO NL! */ additiveOperator NL* multiplicativeExpression)*
    ;

multiplicativeExpression
    : asExpression (/* NO NL! */ multiplicativeOperator NL* asExpression)*
    ;

asExpression
    : prefixUnaryExpression (NL* asOperator NL* type)?
    ;

prefixUnaryExpression
    : unaryPrefix* postfixUnaryExpression
    ;

unaryPrefix
    : annotation
    | labelDefinition
    | prefixUnaryOperator NL*
    ;

postfixUnaryExpression
    : primaryExpression // as an alternative for the correct priority
    | primaryExpression (postfixUnarySuffix)*
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
    : '[' NL* expression (NL* ',' NL* expression)* NL* ']'
    ;

navigationSuffix
    : NL* memberAccessOperator NL* (simpleIdentifier | parenthesizedExpression | 'class')
    ;

callSuffix
    : typeArguments? valueArguments? annotatedLambda
    | typeArguments? valueArguments
    ;

annotatedLambda
    : (annotation | IdentifierAt)* NL* lambdaLiteral
    ;

valueArguments
    : '(' NL* valueArgument? NL* ')'
    | '(' NL* valueArgument (NL* ',' NL* valueArgument)* NL* ')'
    ;

typeArguments
    : '<' NL* typeProjection (NL* ',' NL* typeProjection)* NL* '>'
    ;

typeProjection
    : typeProjectionModifierList? type | '*'
    ;

typeProjectionModifierList
    : varianceAnnotation+
    ;

valueArgument
    : annotation? NL* (simpleIdentifier NL* '=' NL*)? '*'? NL* expression
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
    | ifExpression
    | whenExpression
    | tryExpression
    | jumpExpression
    ;

parenthesizedExpression
    : '(' NL* expression NL* ')'
    ;

collectionLiteral
    : '[' NL* expression (NL* ',' NL* expression)* NL* ']'
    | '[' NL* ']'
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
    : TRIPLE_QUOTE_OPEN (multiLineStringContent | multiLineStringExpression | MultiLineStringQuote)* TRIPLE_QUOTE_CLOSE
    ;

lineStringContent
    : LineStrText
    | LineStrEscapedChar
    | LineStrRef
    ;

lineStringExpression
    : LineStrExprStart expression '}'
    ;

multiLineStringContent
    : MultiLineStrText
    | MultiLineStringQuote
    | MultiLineStrRef
    ;

multiLineStringExpression
    : MultiLineStrExprStart NL* expression NL* '}'
    ;

lambdaLiteral // anonymous functions?
    : LCURL NL* statements NL* RCURL
    | LCURL NL* lambdaParameters NL* ARROW NL* statements NL* '}'
    ;

lambdaParameters
    : lambdaParameter? (NL* COMMA NL* lambdaParameter)*
    ;

lambdaParameter
    : variableDeclaration
    | multiVariableDeclaration (NL* COLON NL* type)?
    ;

anonymousFunction
    : 'fun'
    (NL* type NL* '.')?
    NL* functionValueParameters
    (NL* ':' NL* type)?
    (NL* typeConstraints)?
    (NL* functionBody)?
    ;

functionLiteral
    : lambdaLiteral
    | anonymousFunction
    ;

objectLiteral
    : 'object' NL* ':' NL* delegationSpecifiers (NL* classBody)?
    | 'object' NL* classBody
    ;

thisExpression
    : 'this' AtIdentifier?
    | THIS_AT
    ;

superExpression
    : 'super' ('<' NL* type NL* '>')? AtIdentifier?
    | SUPER_AT
    ;

controlStructureBody
    : block
    | statement
    ;

ifExpression
    : 'if' NL* '(' NL* expression NL* ')' NL* controlStructureBody (';'? NL* 'else' NL* controlStructureBody)?
    | 'if' NL* '(' NL* expression NL* ')' NL* (';' NL*)? 'else' NL* controlStructureBody
    ;

whenExpression
    : 'when' NL* ('(' expression ')')? NL* '{' NL* (whenEntry NL*)* NL* '}'
    ;

whenEntry
    : whenCondition (NL* ',' NL* whenCondition)* NL* '->' NL* controlStructureBody semi?
    | 'else' NL* '->' NL* controlStructureBody semi?
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
    : 'try' NL* block (NL* catchBlock)* (NL* finallyBlock)?
    ;

catchBlock
    : 'catch' NL* '(' catchParameter ')' NL* block
    ;

catchParameter
    : annotation* simpleIdentifier ':' (userType | parenthesizedUserType)
    ;

finallyBlock
    : 'finally' NL* block
    ;

loopStatement
    : forStatement
    | whileStatement
    | doWhileStatement
    ;

forStatement
    : 'for' NL* '(' annotation* (variableDeclaration | multiVariableDeclaration) 'in' expression ')' NL* controlStructureBody?
    ;

whileStatement
    : 'while' NL* '(' expression ')' NL* controlStructureBody
    | 'while' NL* '(' expression ')' NL* ';'
    ;

doWhileStatement
    : 'do' NL* controlStructureBody? NL* 'while' NL* '(' expression ')'
    ;

jumpExpression
    : 'throw' NL* expression
    | ('return' | RETURN_AT) expression?
    | 'continue' | CONTINUE_AT
    | 'break' | BREAK_AT
    ;

callableReference // ?:: here is not an actual operator, it's just a lexer hack to avoid (?: + :) vs (? + ::) ambiguity
    : (receiverType? NL* ('::' | '?::') NL* (simpleIdentifier | 'class'))
    ;

assignmentAndOperator
    : '='
    | '+='
    | '-='
    | '*='
    | '/='
    | '%='
    ;

equalityOperator
    : '!='
    | '!=='
    | '=='
    | '==='
    ;

comparisonOperator
    : '<'
    | '>'
    | '<='
    | '>='
    ;

inOperator
    : 'in' | NOT_IN
    ;

isOperator
    : 'is' | NOT_IS
    ;

additiveOperator
    : '+' | '-'
    ;

multiplicativeOperator
    : '*'
    | '/'
    | '%'
    ;

asOperator
    : 'as'
    | 'as?'
    ;

prefixUnaryOperator
    : '++'
    | '--'
    | '-'
    | '+'
    | '!'
    ;

postfixUnaryOperator
    : '++'
    | '--'
    | '!!'
    ;

memberAccessOperator
    : '.' | '?' /* XXX: no WS here */ '.' | '::'
    ;

modifierList
    : (annotation | modifier)+
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
    : 'enum'
    | 'sealed'
    | 'annotation'
    | 'data'
    | 'inner'
    ;

memberModifier
    : 'override'
    | 'lateinit'
    ;

visibilityModifier
    : 'public'
    | 'private'
    | 'internal'
    | 'protected'
    ;

varianceAnnotation
    : 'in'
    | 'out'
    ;

functionModifier
    : 'tailrec'
    | 'operator'
    | 'infix'
    | 'inline'
    | 'external'
    | 'suspend'
    ;

propertyModifier
    : 'const'
    ;

inheritanceModifier
    : 'abstract'
    | 'final'
    | 'open'
    ;

parameterModifier
    : 'vararg'
    | 'noinline'
    | 'crossinline'
    ;

typeParameterModifier
    : 'reified'
    ;

platformModifier
    : 'expect'
    | 'actual'
    ;

labelDefinition
    : IdentifierAt NL*
    ;

annotation
    : (singleAnnotation | multiAnnotation) NL*
    ;

singleAnnotation
    : annotationUseSiteTarget ':' NL* unescapedAnnotation
    | AtIdentifier (NL* '.' simpleIdentifier)* typeArguments? valueArguments?
    ;

multiAnnotation
    : annotationUseSiteTarget ':' '[' unescapedAnnotation+ ']'
    | '@[' unescapedAnnotation+ ']'
    ;

annotationUseSiteTarget
    : '@field'
    | '@file'
    | '@property'
    | '@get'
    | '@set'
    | '@receiver'
    | '@param'
    | '@setparam'
    | '@delegate'
    ;

unescapedAnnotation
    : identifier typeArguments? valueArguments?
    ;

simpleIdentifier
    : Identifier //soft keywords:
    | 'abstract'
    | 'annotation'
    | 'by'
    | 'catch'
    | 'companion'
    | 'constructor'
    | 'crossinline'
    | 'data'
    | 'dynamic'
    | 'enum'
    | 'external'
    | 'final'
    | 'finally'
    | 'get'
    | 'import'
    | 'infix'
    | 'init'
    | 'inline'
    | 'inner'
    | 'internal'
    | 'lateinit'
    | 'noinline'
    | 'open'
    | 'operator'
    | 'out'
    | 'override'
    | 'private'
    | 'protected'
    | 'public'
    | 'reified'
    | 'sealed'
    | 'tailrec'
    | 'set'
    | 'vararg'
    | 'where'
    | 'expect'
    | 'actual'
    | 'const'
    | 'suspend'
    ;

identifier
    : simpleIdentifier (NL* '.' simpleIdentifier)*
    ;

shebangLine
    : ShebangLine
    ;

semi
    : (';' | NL) NL* // actually, it's WS or comment between ';', here it's handled in lexer (see ;; token)
    | EOF;
semis // writing this as "semi+" sends antlr into infinite loop or smth
    : (';' | NL)+
    | EOF
    ;
