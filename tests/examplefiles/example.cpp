/***************************************************************************
                    ansigenerator.cpp  -  description
                             -------------------
    begin                : Jul 5 2004
    copyright            : (C) 2004 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "ansigenerator.h"

using namespace std;

namespace highlight {


string  AnsiGenerator::getOpenTag(const string&font,
                                  const string&fgCol, const string&bgCol) {
    ostringstream s;
    s  << "\033["<<font;
    if (!fgCol.empty())
        s<<";"<<fgCol;
    if (!bgCol.empty())
        s<<";"<<bgCol;
    s << "m";
    return  s.str();
}


AnsiGenerator::AnsiGenerator(const string &colourTheme)
        : CodeGenerator(colourTheme) {
    styleTagOpen.push_back("");
    styleTagOpen.push_back(getOpenTag("00", "31")); //str
    styleTagOpen.push_back(getOpenTag("00", "34"));//number
    styleTagOpen.push_back(getOpenTag("00", "34"));//sl comment
    styleTagOpen.push_back(getOpenTag("00", "34"));//ml comment
    styleTagOpen.push_back(getOpenTag("00", "35"));//escapeChar
    styleTagOpen.push_back(getOpenTag("00", "35"));//directive
    styleTagOpen.push_back(getOpenTag("01", "31"));//directive string
    styleTagOpen.push_back(getOpenTag("00", "30"));//linenum
    styleTagOpen.push_back(getOpenTag("01", "00"));//symbol

    styleTagClose.push_back("");
    for (int i=1;i<NUMBER_BUILTIN_STYLES; i++) {
        styleTagClose.push_back("\033[m");
    }
    newLineTag = "\n";
    spacer = " ";
}

AnsiGenerator::AnsiGenerator() {}
AnsiGenerator::~AnsiGenerator() {}

string AnsiGenerator::getHeader(const string & title) {
    return string();
}

void AnsiGenerator::printBody() {
    processRootState();
}

string AnsiGenerator::getFooter() {
    return string();
}

string AnsiGenerator::maskCharacter(unsigned char c) {
    string m;
    m+=c;
    return m;
}

string AnsiGenerator::getMatchingOpenTag(unsigned int styleID) {
    return (styleID)?getOpenTag("01", "32", ""):getOpenTag("00", "33");
}

string AnsiGenerator::getMatchingCloseTag(unsigned int styleID) {
    return "\033[m";
}

}
/***************************************************************************
                         ansicode.h  -  description
                             -------------------
    begin                : Jul 5 2004
    copyright            : (C) 2004 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef ANSIGENERATOR_H
#define ANSIGENERATOR_H

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

#include "codegenerator.h"
#include "charcodes.h"
#include "version.h"

namespace highlight {

/**
   \brief This class generates ANSI escape sequences.

   It contains information about the resulting document structure (document
   header and footer), the colour system, white space handling and text
   formatting attributes.

* @author Andre Simon
*/

class AnsiGenerator : public highlight::CodeGenerator
  {
  public:

   /** Constructor
     \param colourTheme Name of Colour theme to use
    */
    AnsiGenerator( const string &colourTheme);
    AnsiGenerator();
    ~AnsiGenerator();

   /** prints document header
       \param  title Title of the document
    */
    string getHeader(const string & title);

    /** Prints document footer*/
    string getFooter();

    /** Prints document body*/
    void printBody();

  private:

    /** \return escaped character*/
    virtual string maskCharacter(unsigned char );


    /** gibt ANSI-"Tags" zurueck (Farbindex+bold+kursiv)*/
    string getOpenTag(const string&font,
                      const string&fgCol, const string&bgCol="");



    string getMatchingOpenTag(unsigned int styleID);
    string getMatchingCloseTag(unsigned int styleID);
  };

}
#endif
/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * ASBeautifier.cpp
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 *
 * Patches:
 * 18 March 1999 - Brian Rampel -
 *       Fixed inverse insertion of spaces vs. tabs when in -t mode.
 * 08 may 2004 
 *       applied ASBeautifier.cpp.BITFIELD.patch.bz2
 */

#include "compiler_defines.h"
#include "ASBeautifier.h"

#include <vector>
#include <string>
#include <cctype>
#include <algorithm>
#include <iostream>


#define INIT_CONTAINER(container, value)     {if ( (container) != NULL ) delete (container); (container) = (value); }
#define DELETE_CONTAINER(container)          {if ( (container) != NULL ) delete (container) ; }

#ifdef USES_NAMESPACE
using namespace std;
#endif




#ifdef USES_NAMESPACE
namespace astyle
  {
#endif

  bool ASBeautifier::calledInitStatic = false;

  vector<const string*> ASBeautifier::headers;
  vector<const string*> ASBeautifier::nonParenHeaders;
  vector<const string*> ASBeautifier::preBlockStatements;
  vector<const string*> ASBeautifier::assignmentOperators;
  vector<const string*> ASBeautifier::nonAssignmentOperators;

  /*
   * initialize the static vars
   */
  void ASBeautifier::initStatic()
  {
    if (calledInitStatic)
      return;

    calledInitStatic = true;

    headers.push_back(&AS_IF);
    headers.push_back(&AS_ELSE);
    headers.push_back(&AS_FOR);
    headers.push_back(&AS_WHILE);
    headers.push_back(&AS_DO);
    headers.push_back(&AS_TRY);
    headers.push_back(&AS_CATCH);
    headers.push_back(&AS_FINALLY);
    headers.push_back(&AS_SYNCHRONIZED);
    headers.push_back(&AS_SWITCH);
    headers.push_back(&AS_CASE);
    headers.push_back(&AS_DEFAULT);
    headers.push_back(&AS_FOREACH);
    headers.push_back(&AS_LOCK);
    headers.push_back(&AS_UNSAFE);
    headers.push_back(&AS_FIXED);
    headers.push_back(&AS_GET);
    headers.push_back(&AS_SET);
    headers.push_back(&AS_ADD);
    headers.push_back(&AS_REMOVE);
    //headers.push_back(&AS_PUBLIC);
    //headers.push_back(&AS_PRIVATE);
    //headers.push_back(&AS_PROTECTED);

    //headers.push_back(&AS_OPERATOR);
    headers.push_back(&AS_TEMPLATE);
    headers.push_back(&AS_CONST);
    /**/
    headers.push_back(&AS_STATIC);
    headers.push_back(&AS_EXTERN);

    nonParenHeaders.push_back(&AS_ELSE);
    nonParenHeaders.push_back(&AS_DO);
    nonParenHeaders.push_back(&AS_TRY);
    nonParenHeaders.push_back(&AS_FINALLY);
    nonParenHeaders.push_back(&AS_STATIC);
    nonParenHeaders.push_back(&AS_CONST);
    nonParenHeaders.push_back(&AS_EXTERN);
    nonParenHeaders.push_back(&AS_CASE);
    nonParenHeaders.push_back(&AS_DEFAULT);
    nonParenHeaders.push_back(&AS_UNSAFE);
    nonParenHeaders.push_back(&AS_GET);
    nonParenHeaders.push_back(&AS_SET);
    nonParenHeaders.push_back(&AS_ADD);
    nonParenHeaders.push_back(&AS_REMOVE);



    nonParenHeaders.push_back(&AS_PUBLIC);
    nonParenHeaders.push_back(&AS_PRIVATE);
    nonParenHeaders.push_back(&AS_PROTECTED);
    nonParenHeaders.push_back(&AS_TEMPLATE);
    nonParenHeaders.push_back(&AS_CONST);
    ///    nonParenHeaders.push_back(&AS_ASM);

    preBlockStatements.push_back(&AS_CLASS);
    preBlockStatements.push_back(&AS_STRUCT);
    preBlockStatements.push_back(&AS_UNION);
    preBlockStatements.push_back(&AS_INTERFACE);
    preBlockStatements.push_back(&AS_NAMESPACE);
    preBlockStatements.push_back(&AS_THROWS);
    preBlockStatements.push_back(&AS_EXTERN);

    assignmentOperators.push_back(&AS_ASSIGN);
    assignmentOperators.push_back(&AS_PLUS_ASSIGN);
    assignmentOperators.push_back(&AS_MINUS_ASSIGN);
    assignmentOperators.push_back(&AS_MULT_ASSIGN);
    assignmentOperators.push_back(&AS_DIV_ASSIGN);
    assignmentOperators.push_back(&AS_MOD_ASSIGN);
    assignmentOperators.push_back(&AS_OR_ASSIGN);
    assignmentOperators.push_back(&AS_AND_ASSIGN);
    assignmentOperators.push_back(&AS_XOR_ASSIGN);
    assignmentOperators.push_back(&AS_GR_GR_GR_ASSIGN);
    assignmentOperators.push_back(&AS_GR_GR_ASSIGN);
    assignmentOperators.push_back(&AS_LS_LS_LS_ASSIGN);
    assignmentOperators.push_back(&AS_LS_LS_ASSIGN);

    assignmentOperators.push_back(&AS_RETURN);

    nonAssignmentOperators.push_back(&AS_EQUAL);
    nonAssignmentOperators.push_back(&AS_PLUS_PLUS);
    nonAssignmentOperators.push_back(&AS_MINUS_MINUS);
    nonAssignmentOperators.push_back(&AS_NOT_EQUAL);
    nonAssignmentOperators.push_back(&AS_GR_EQUAL);
    nonAssignmentOperators.push_back(&AS_GR_GR_GR);
    nonAssignmentOperators.push_back(&AS_GR_GR);
    nonAssignmentOperators.push_back(&AS_LS_EQUAL);
    nonAssignmentOperators.push_back(&AS_LS_LS_LS);
    nonAssignmentOperators.push_back(&AS_LS_LS);
    nonAssignmentOperators.push_back(&AS_ARROW);
    nonAssignmentOperators.push_back(&AS_AND);
    nonAssignmentOperators.push_back(&AS_OR);
  }

  /**
   * ASBeautifier's constructor
   */
  ASBeautifier::ASBeautifier()
  {
    initStatic();

    waitingBeautifierStack = NULL;
    activeBeautifierStack = NULL;
    waitingBeautifierStackLengthStack = NULL;
    activeBeautifierStackLengthStack = NULL;

    headerStack  = NULL;
    tempStacks = NULL;
    blockParenDepthStack = NULL;
    blockStatementStack = NULL;
    parenStatementStack = NULL;
    bracketBlockStateStack = NULL;
    inStatementIndentStack = NULL;
    inStatementIndentStackSizeStack = NULL;
    parenIndentStack = NULL;
    sourceIterator = NULL;

    isMinimalConditinalIndentSet = false;
    shouldForceTabIndentation = false;

    setSpaceIndentation(4);
    setMaxInStatementIndentLength(40);
    setClassIndent(false);
    setSwitchIndent(false);
    setCaseIndent(false);
    setBlockIndent(false);
    setBracketIndent(false);
    setNamespaceIndent(false);
    setLabelIndent(false);
    setEmptyLineFill(false);
    setCStyle();
    setPreprocessorIndent(false);
  }

  ASBeautifier::ASBeautifier(const ASBeautifier &other)
  {
    waitingBeautifierStack = NULL;
    activeBeautifierStack = NULL;
    waitingBeautifierStackLengthStack = NULL;
    activeBeautifierStackLengthStack = NULL;

    headerStack  = new vector<const string*>;
    *headerStack = *other.headerStack;

    tempStacks = new vector< vector<const string*>* >;
    vector< vector<const string*>* >::iterator iter;
    for (iter = other.tempStacks->begin();
         iter != other.tempStacks->end();
         ++iter)
      {
        vector<const string*> *newVec = new vector<const string*>;
        *newVec = **iter;
        tempStacks->push_back(newVec);
      }
    blockParenDepthStack = new vector<int>;
    *blockParenDepthStack = *other.blockParenDepthStack;

    blockStatementStack = new vector<bool>;
    *blockStatementStack = *other.blockStatementStack;

    parenStatementStack =  new vector<bool>;
    *parenStatementStack = *other.parenStatementStack;

    bracketBlockStateStack = new vector<bool>;
    *bracketBlockStateStack = *other.bracketBlockStateStack;

    inStatementIndentStack = new vector<int>;
    *inStatementIndentStack = *other.inStatementIndentStack;

    inStatementIndentStackSizeStack = new vector<int>;
    *inStatementIndentStackSizeStack = *other.inStatementIndentStackSizeStack;

    parenIndentStack = new vector<int>;
    *parenIndentStack = *other.parenIndentStack;

    sourceIterator = other.sourceIterator;

    indentString = other.indentString;
    currentHeader = other.currentHeader;
    previousLastLineHeader = other.previousLastLineHeader;
    immediatelyPreviousAssignmentOp = other.immediatelyPreviousAssignmentOp;
    isInQuote = other.isInQuote;
    isInComment = other.isInComment;
    isInCase = other.isInCase;
    isInQuestion = other.isInQuestion;
    isInStatement =other. isInStatement;
    isInHeader = other.isInHeader;
    isCStyle = other.isCStyle;
    isInOperator = other.isInOperator;
    isInTemplate = other.isInTemplate;
    isInConst = other.isInConst;
    classIndent = other.classIndent;
    isInClassHeader = other.isInClassHeader;
    isInClassHeaderTab = other.isInClassHeaderTab;
    switchIndent = other.switchIndent;
    caseIndent = other.caseIndent;
    namespaceIndent = other.namespaceIndent;
    bracketIndent = other.bracketIndent;
    blockIndent = other.blockIndent;
    labelIndent = other.labelIndent;
    preprocessorIndent = other.preprocessorIndent;
    parenDepth = other.parenDepth;
    indentLength = other.indentLength;
    blockTabCount = other.blockTabCount;
    leadingWhiteSpaces = other.leadingWhiteSpaces;
    maxInStatementIndent = other.maxInStatementIndent;
    templateDepth = other.templateDepth;
    quoteChar = other.quoteChar;
    prevNonSpaceCh = other.prevNonSpaceCh;
    currentNonSpaceCh = other.currentNonSpaceCh;
    currentNonLegalCh = other.currentNonLegalCh;
    prevNonLegalCh = other.prevNonLegalCh;
    isInConditional = other.isInConditional;
    minConditionalIndent = other.minConditionalIndent;
    prevFinalLineSpaceTabCount = other.prevFinalLineSpaceTabCount;
    prevFinalLineTabCount = other.prevFinalLineTabCount;
    emptyLineFill = other.emptyLineFill;
    probationHeader = other.probationHeader;
    isInDefine = other.isInDefine;
    isInDefineDefinition = other.isInDefineDefinition;
    backslashEndsPrevLine = other.backslashEndsPrevLine;
    defineTabCount = other.defineTabCount;
  }

  /**
   * ASBeautifier's destructor
   */
  ASBeautifier::~ASBeautifier()
  {
    DELETE_CONTAINER( headerStack );
    DELETE_CONTAINER( tempStacks );
    DELETE_CONTAINER( blockParenDepthStack );
    DELETE_CONTAINER( blockStatementStack );
    DELETE_CONTAINER( parenStatementStack );
    DELETE_CONTAINER( bracketBlockStateStack );
    DELETE_CONTAINER( inStatementIndentStack );
    DELETE_CONTAINER( inStatementIndentStackSizeStack );
    DELETE_CONTAINER( parenIndentStack );

    // DELETE_CONTAINER( sourceIterator );
  }

  /**
   * initialize the ASBeautifier.
   *
   * init() should be called every time a ABeautifier object is to start
   * beautifying a NEW source file.
   * init() recieves a pointer to a DYNAMICALLY CREATED ASSourceIterator object
   * that will be used to iterate through the source code. This object will be
   * deleted during the ASBeautifier's destruction, and thus should not be
   * deleted elsewhere.
   *
   * @param iter     a pointer to the DYNAMICALLY CREATED ASSourceIterator object.
   */
  void ASBeautifier::init(ASSourceIterator *iter)

  {
    sourceIterator = iter;
    init();
  }

  /**
   * initialize the ASBeautifier.
   */
  void ASBeautifier::init()
  {
    INIT_CONTAINER( waitingBeautifierStack,  new vector<ASBeautifier*> );
    INIT_CONTAINER( activeBeautifierStack,  new vector<ASBeautifier*> );

    INIT_CONTAINER( waitingBeautifierStackLengthStack, new vector<int> );
    INIT_CONTAINER( activeBeautifierStackLengthStack, new vector<int> );

    INIT_CONTAINER( headerStack,  new vector<const string*> );
    INIT_CONTAINER( tempStacks, new vector< vector<const string*>* > );
    tempStacks->push_back(new vector<const string*>);

    INIT_CONTAINER( blockParenDepthStack, new vector<int> );
    INIT_CONTAINER( blockStatementStack, new vector<bool> );
    INIT_CONTAINER( parenStatementStack, new vector<bool> );

    INIT_CONTAINER( bracketBlockStateStack, new vector<bool> );
    bracketBlockStateStack->push_back(true);

    INIT_CONTAINER( inStatementIndentStack, new vector<int> );
    INIT_CONTAINER( inStatementIndentStackSizeStack, new vector<int> );
    inStatementIndentStackSizeStack->push_back(0);
    INIT_CONTAINER( parenIndentStack, new vector<int> );

    immediatelyPreviousAssignmentOp = NULL;
    previousLastLineHeader = NULL;

    isInQuote = false;
    isInComment = false;
    isInStatement = false;
    isInCase = false;
    isInQuestion = false;
    isInClassHeader = false;
    isInClassHeaderTab = false;
    isInHeader = false;
    isInOperator = false;
    isInTemplate = false;
    isInConst = false;
    isInConditional = false;
    templateDepth = 0;
    parenDepth=0;
    blockTabCount = 0;
    leadingWhiteSpaces = 0;
    prevNonSpaceCh = '{';
    currentNonSpaceCh = '{';
    prevNonLegalCh = '{';
    currentNonLegalCh = '{';
    prevFinalLineSpaceTabCount = 0;
    prevFinalLineTabCount = 0;
    probationHeader = NULL;
    backslashEndsPrevLine = false;
    isInDefine = false;
    isInDefineDefinition = false;
    defineTabCount = 0;
  }

  /**
   * set indentation style to ANSI C/C++.  
   */
  void ASBeautifier::setCStyle()
  {
    isCStyle = true;
  }

  /**
   * set indentation style to Java / K&R.  
   */
  void ASBeautifier::setJavaStyle()
  {
    isCStyle = false;
  }

  /**
   * indent using one tab per indentation
   */
  void ASBeautifier::setTabIndentation(int length, bool forceTabs)
  {
    indentString = "\t";
    indentLength = length;
    shouldForceTabIndentation = forceTabs;

    if (!isMinimalConditinalIndentSet)
      minConditionalIndent = indentLength * 2;
  }

  /**
   
   * indent using a number of spaces per indentation.
   *
   * @param   length     number of spaces per indent.
   */
  void ASBeautifier::setSpaceIndentation(int length)
  {
    indentString=string(length, ' ');
    indentLength = length;

    if (!isMinimalConditinalIndentSet)
      minConditionalIndent = indentLength * 2;
  }

  /**
   * set the maximum indentation between two lines in a multi-line statement.
   *
   * @param   max     maximum indentation length.
   */
  void ASBeautifier::setMaxInStatementIndentLength(int max)
  {
    maxInStatementIndent = max;
  }

  /**
   * set the minimum indentation between two lines in a multi-line condition.
   *
   * @param   min     minimal indentation length.
   */
  void ASBeautifier::setMinConditionalIndentLength(int min)
  {
    minConditionalIndent = min;
    isMinimalConditinalIndentSet = true;
  }

  /**
   * set the state of the bracket indentation option. If true, brackets will 
   * be indented one additional indent.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setBracketIndent(bool state)
  {
    bracketIndent = state;
  }

  /**
   * set the state of the block indentation option. If true, entire blocks 
   * will be indented one additional indent, similar to the GNU indent style.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setBlockIndent(bool state)
  {
    if (state)
      setBracketIndent(false); // so that we don't have both bracket and block indent
    blockIndent = state;
  }

  /**
   * set the state of the class indentation option. If true, C++ class
   * definitions will be indented one additional indent.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setClassIndent(bool state)
  {
    classIndent = state;
  }

  /**
   * set the state of the switch indentation option. If true, blocks of 'switch' 
   * statements will be indented one additional indent.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setSwitchIndent(bool state)
  {
    switchIndent = state;
  }

  /**
   * set the state of the case indentation option. If true, lines of 'case' 
   * statements will be indented one additional indent.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setCaseIndent(bool state)
  {
    caseIndent = state;
  }
  /**
   * set the state of the namespace indentation option. 
   * If true, blocks of 'namespace' statements will be indented one 
   * additional indent. Otherwise, NO indentation will be added.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setNamespaceIndent(bool state)
  {
    namespaceIndent = state;
  }

  /**
   * set the state of the label indentation option. 
   * If true, labels will be indented one indent LESS than the
   * current indentation level.
   * If false, labels will be flushed to the left with NO
   * indent at all.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setLabelIndent(bool state)
  {
    labelIndent = state;
  }

  /**
   * set the state of the preprocessor indentation option. 
   * If true, multiline #define statements will be indented.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setPreprocessorIndent(bool state)
  {
    preprocessorIndent = state;
  }

  /**
   * set the state of the empty line fill option. 
   * If true, empty lines will be filled with the whitespace.
   * of their previous lines.
   * If false, these lines will remain empty.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setEmptyLineFill(bool state)
  {
    emptyLineFill = state;
  }

  /**
   * check if there are any indented lines ready to be read by nextLine()
   *
   * @return    are there any indented lines ready?
   */
  bool ASBeautifier::hasMoreLines() const
    {
      return sourceIterator->hasMoreLines();
    }

  /**
   * get the next indented line.
   *
   * @return    indented line.
   */
  string ASBeautifier::nextLine()
  {
    return beautify(sourceIterator->nextLine());
  }

  /**
   * beautify a line of source code.
   * every line of source code in a source code file should be sent
   * one after the other to the beautify method.
   *
   * @return      the indented line.
   * @param originalLine       the original unindented line.
   */
  string ASBeautifier::beautify(const string &originalLine)
  {
    string line;
    bool isInLineComment = false;
    bool lineStartsInComment = false;
    bool isInClass = false;
    bool isInSwitch = false;
    bool isImmediatelyAfterConst = false;
    bool isSpecialChar = false;

    char ch = ' ';
    char prevCh;
    string outBuffer; // the newly idented line is bufferd here
    int tabCount = 0;
    const string *lastLineHeader = NULL;
    bool closingBracketReached = false;
    int spaceTabCount = 0;
    char tempCh;
    unsigned int headerStackSize = headerStack->size();
    //bool isLineInStatement = isInStatement;
    bool shouldIndentBrackettedLine = true;
    int lineOpeningBlocksNum = 0;
    int lineClosingBlocksNum = 0;
    bool previousLineProbation = (probationHeader != NULL);
    unsigned int i;

    currentHeader = NULL;

    lineStartsInComment = isInComment;

    // handle and remove white spaces around the line:
    // If not in comment, first find out size of white space before line,
    // so that possible comments starting in the line continue in
    // relation to the preliminary white-space.
    if (!isInComment)
      {
        leadingWhiteSpaces = 0;
        while (leadingWhiteSpaces<originalLine.length() && originalLine[leadingWhiteSpaces] <= 0x20)
          leadingWhiteSpaces++;

        line = trim(originalLine);
      }
    else
      {
        unsigned int trimSize;
        for (trimSize=0;
             trimSize < originalLine.length() && trimSize<leadingWhiteSpaces && originalLine[trimSize] <= 0x20 ;
             trimSize++)
          ;
        line = originalLine.substr(trimSize);
      }


    if (line.length() == 0)
      {
        if (emptyLineFill)
          return preLineWS(prevFinalLineSpaceTabCount, prevFinalLineTabCount);
        else
          return line;
      }

    // handle preprocessor commands

    if (isCStyle && !isInComment && (line[0] == '#' || backslashEndsPrevLine))
      {
        if (line[0] == '#')
          {
            string preproc = trim(string(line.c_str() + 1));


            // When finding a multi-lined #define statement, the original beautifier
            // 1. sets its isInDefineDefinition flag
            // 2. clones a new beautifier that will be used for the actual indentation
            //    of the #define. This clone is put into the activeBeautifierStack in order
            //    to be called for the actual indentation.
            // The original beautifier will have isInDefineDefinition = true, isInDefine = false
            // The cloned beautifier will have   isInDefineDefinition = true, isInDefine = true
            if (preprocessorIndent && preproc.COMPARE(0, 6, string("define")) == 0 &&  line[line.length() - 1] == '\\')
              {
                if (!isInDefineDefinition)
                  {
                    ASBeautifier *defineBeautifier;

                    // this is the original beautifier
                    isInDefineDefinition = true;

                    // push a new beautifier into the active stack
                    // this breautifier will be used for the indentation of this define
                    defineBeautifier = new ASBeautifier(*this);
                    //defineBeautifier->init();
                    //defineBeautifier->isInDefineDefinition = true;
                    //defineBeautifier->beautify("");
                    activeBeautifierStack->push_back(defineBeautifier);
                  }
                else
                  {
                    // the is the cloned beautifier that is in charge of indenting the #define.
                    isInDefine = true;
                  }
              }
            else if (preproc.COMPARE(0, 2, string("if")) == 0)
              {
                // push a new beautifier into the stack
                waitingBeautifierStackLengthStack->push_back(waitingBeautifierStack->size());
                activeBeautifierStackLengthStack->push_back(activeBeautifierStack->size());
                waitingBeautifierStack->push_back(new ASBeautifier(*this));
              }
            else if (preproc.COMPARE(0, 4/*2*/, string("else")) == 0)
              {
                if (!waitingBeautifierStack->empty())
                  {
                    // MOVE current waiting beautifier to active stack.
                    activeBeautifierStack->push_back(waitingBeautifierStack->back());
                    waitingBeautifierStack->pop_back();
                  }
              }
            else if (preproc.COMPARE(0, 4, string("elif")) == 0)
              {
                if (!waitingBeautifierStack->empty())
                  {
                    // append a COPY current waiting beautifier to active stack, WITHOUT deleting the original.
                    activeBeautifierStack->push_back( new ASBeautifier( *(waitingBeautifierStack->back()) ) );
                  }
              }
            else if (preproc.COMPARE(0, 5, string("endif")) == 0)
              {
                unsigned int stackLength;
                ASBeautifier *beautifier;

                if (!waitingBeautifierStackLengthStack->empty())
                  {
                    stackLength = waitingBeautifierStackLengthStack->back();
                    waitingBeautifierStackLengthStack->pop_back();
                    while (waitingBeautifierStack->size() > stackLength)
                      {
                        beautifier = waitingBeautifierStack->back();
                        waitingBeautifierStack->pop_back();
                        delete beautifier;
                      }
                  }

                if (!activeBeautifierStackLengthStack->empty())
                  {
                    stackLength = activeBeautifierStackLengthStack->back();
                    activeBeautifierStackLengthStack->pop_back();
                    while (activeBeautifierStack->size() > stackLength)
                      {
                        beautifier = activeBeautifierStack->back();
                        activeBeautifierStack->pop_back();
                        delete beautifier;
                      }
                  }


              }
          }

        // check if the last char is a backslash
        if(line.length() > 0)
          backslashEndsPrevLine = (line[line.length() - 1] == '\\');
        else
          backslashEndsPrevLine = false;

        // check if this line ends a multi-line #define
        // if so, use the #define's cloned beautifier for the line's indentation
        // and then remove it from the active beautifier stack and delete it.
        if (!backslashEndsPrevLine && isInDefineDefinition && !isInDefine)
          {
            string beautifiedLine;
            ASBeautifier *defineBeautifier;

            isInDefineDefinition = false;
            defineBeautifier = activeBeautifierStack->back();
            activeBeautifierStack->pop_back();

            beautifiedLine = defineBeautifier->beautify(line);
            delete defineBeautifier;
            return beautifiedLine;
          }

        // unless this is a multi-line #define, return this precompiler line as is.
        if (!isInDefine && !isInDefineDefinition)
          return originalLine;
      }

    // if there exists any worker beautifier in the activeBeautifierStack,
    // then use it instead of me to indent the current line.
    if (!isInDefine && activeBeautifierStack != NULL && !activeBeautifierStack->empty())
      {
        return activeBeautifierStack->back()->beautify(line);
      }

    // calculate preliminary indentation based on data from past lines
    if (!inStatementIndentStack->empty())
      spaceTabCount = inStatementIndentStack->back();


    for (i=0; i<headerStackSize; i++)
      {
        isInClass = false;

        if (blockIndent || (!(i>0 && (*headerStack)[i-1] != &AS_OPEN_BRACKET
                              && (*headerStack)[i] == &AS_OPEN_BRACKET)))
          ++tabCount;

        if (isCStyle && !namespaceIndent && i >= 1
            && (*headerStack)[i-1] == &AS_NAMESPACE
            && (*headerStack)[i] == &AS_OPEN_BRACKET)
          --tabCount;

        if (isCStyle && i >= 1
            && (*headerStack)[i-1] == &AS_CLASS
            && (*headerStack)[i] == &AS_OPEN_BRACKET )
          {
            if (classIndent)
              ++tabCount;
            isInClass = true;
          }

        // is the switchIndent option is on, indent switch statements an additional indent.
        else if (switchIndent && i > 1 &&
                 (*headerStack)[i-1] == &AS_SWITCH &&
                 (*headerStack)[i] == &AS_OPEN_BRACKET
                )
          {
            ++tabCount;
            isInSwitch = true;
          }

      }

    if (!lineStartsInComment
        && isCStyle
        && isInClass
        && classIndent
        && headerStackSize >= 2
        &&(*headerStack)[headerStackSize-2] == &AS_CLASS
        && (*headerStack)[headerStackSize-1] == &AS_OPEN_BRACKET
        && line[0] == '}')
      --tabCount;

    else if (!lineStartsInComment
             && isInSwitch
             && switchIndent
             && headerStackSize >= 2
             && (*headerStack)[headerStackSize-2] == &AS_SWITCH
             && (*headerStack)[headerStackSize-1] == &AS_OPEN_BRACKET
             && line[0] == '}')
      --tabCount;

    if (isInClassHeader)
      {
        isInClassHeaderTab = true;
        tabCount += 2;
      }

    if (isInConditional)
      {
        --tabCount;
      }


    // parse characters in the current line.

    for (i=0; i<line.length(); i++)
      {
        tempCh = line[i];

        prevCh = ch;
        ch = tempCh;

        outBuffer.append(1, ch);

        if (isWhiteSpace(ch))
          continue;


        // handle special characters (i.e. backslash+character such as \n, \t, ...)
        if (isSpecialChar)
          {
            isSpecialChar = false;
            continue;
          }
        if (!(isInComment || isInLineComment) && line.COMPARE(i, 2, string("\\\\")) == 0)
          {
            outBuffer.append(1, '\\');
            i++;
            continue;
          }
        if (!(isInComment || isInLineComment) && ch=='\\')
          {
            isSpecialChar = true;
            continue;
          }

        // handle quotes (such as 'x' and "Hello Dolly")
        if (!(isInComment || isInLineComment) && (ch=='"' || ch=='\''))
          if (!isInQuote)
            {
              quoteChar = ch;
              isInQuote = true;
            }
          else if (quoteChar == ch)
            {
              isInQuote = false;
              isInStatement = true;
              continue;
            }
        if (isInQuote)
          continue;

        // handle comments

        if ( !(isInComment || isInLineComment) && line.COMPARE(i, 2, AS_OPEN_LINE_COMMENT) == 0 )
          {
            isInLineComment = true;
            outBuffer.append(1, '/');
            i++;
            continue;
          }
        else if ( !(isInComment || isInLineComment) && line.COMPARE(i, 2, AS_OPEN_COMMENT) == 0 )
          {
            isInComment = true;
            outBuffer.append(1, '*');
            i++;
            continue;
          }
        else if ( (isInComment || isInLineComment) && line.COMPARE(i, 2, AS_CLOSE_COMMENT) == 0 )
          {
            isInComment = false;
            outBuffer.append(1, '/');
            i++;
            continue;
          }

        if (isInComment||isInLineComment)
          continue;

        // if we have reached this far then we are NOT in a comment or string of special character...

        if (probationHeader != NULL)
          {
            if ( ((probationHeader == &AS_STATIC || probationHeader == &AS_CONST) && ch == '{')
                 || (probationHeader == &AS_SYNCHRONIZED && ch == '('))
              {
                // insert the probation header as a new header
                isInHeader = true;
                headerStack->push_back(probationHeader);

                // handle the specific probation header
                isInConditional = (probationHeader == &AS_SYNCHRONIZED);
                if (probationHeader == &AS_CONST)
                  isImmediatelyAfterConst = true;
                //  isInConst = true;
                /* TODO:
                 * There is actually no more need for the global isInConst variable.
                               * The only reason for checking const is to see if there is a const
                 * immediately before an open-bracket.
                 * Since CONST is now put into probation and is checked during itspost-char,
                 * isImmediatelyAfterConst can be set by its own...
                 */

                isInStatement = false;
                // if the probation comes from the previous line, then indent by 1 tab count.
                if (previousLineProbation && ch == '{')
                  tabCount++;
                previousLineProbation = false;
              }

            // dismiss the probation header
            probationHeader = NULL;
          }

        prevNonSpaceCh = currentNonSpaceCh;
        currentNonSpaceCh = ch;
        if (!isLegalNameChar(ch) && ch != ',' && ch != ';' )
          {
            prevNonLegalCh = currentNonLegalCh;
            currentNonLegalCh = ch;
          }

        //if (isInConst)
        //{
        //    isInConst = false;
        //    isImmediatelyAfterConst = true;
        //}

        if (isInHeader)
          {
            isInHeader = false;
            currentHeader = headerStack->back();
          }
        else
          currentHeader = NULL;

        if (isCStyle && isInTemplate
            && (ch == '<' || ch == '>')
            &&  findHeader(line, i, nonAssignmentOperators) == NULL) //;
          {
            if (ch == '<')
              {
                ++templateDepth;
              }
            else if (ch == '>')
              {
                if (--templateDepth <= 0)
                  {
                    if (isInTemplate)
                      ch = ';';
                    else
                      ch = 't';
                    isInTemplate = false;
                    templateDepth = 0;
                  }

              }
          }

        // handle parenthesies
        if (ch == '(' || ch == '[' || ch == ')' || ch == ']')
          {
            if (ch == '(' || ch == '[')
              {
                if (parenDepth == 0)
                  {
                    parenStatementStack->push_back(isInStatement);
                    isInStatement = true;
                  }
                parenDepth++;

                inStatementIndentStackSizeStack->push_back(inStatementIndentStack->size());

                if (currentHeader != NULL)
                  registerInStatementIndent(line, i, spaceTabCount, minConditionalIndent/*indentLength*2*/, true);
                else
                  registerInStatementIndent(line, i, spaceTabCount, 0, true);
              }
            else if (ch == ')' || ch == ']')
              {
                parenDepth--;
                if (parenDepth == 0)
                  {
                    isInStatement = parenStatementStack->back();
                    parenStatementStack->pop_back();
                    ch = ' ';

                    isInConditional = false;
                  }

                if (!inStatementIndentStackSizeStack->empty())
                  {
                    unsigned int previousIndentStackSize = inStatementIndentStackSizeStack->back();
                    inStatementIndentStackSizeStack->pop_back();
                    while (previousIndentStackSize < inStatementIndentStack->size())
                      inStatementIndentStack->pop_back();

                    if (!parenIndentStack->empty())
                      {
                        int poppedIndent = parenIndentStack->back();
                        parenIndentStack->pop_back();

                        if (i == 0)
                          spaceTabCount = poppedIndent;
                      }
                  }
              }

            continue;
          }


        if (ch == '{')
          {
            bool isBlockOpener = false;

            // first, check if '{' is a block-opener or an static-array opener
            isBlockOpener = ( (prevNonSpaceCh == '{' && bracketBlockStateStack->back())
                              || prevNonSpaceCh == '}'
                              || prevNonSpaceCh == ')'
                              || prevNonSpaceCh == ';'
                              || isInClassHeader
                              || isBlockOpener
                              || isImmediatelyAfterConst
                              || (isInDefine &&
                                  (prevNonSpaceCh == '('
                                   || prevNonSpaceCh == '_'
                                   || isalnum(prevNonSpaceCh))) );

            isInClassHeader = false;
            if (!isBlockOpener && currentHeader != NULL)
              {
                for (unsigned int n=0; n < nonParenHeaders.size(); n++)
                  if (currentHeader == nonParenHeaders[n])
                    {
                      isBlockOpener = true;
                      break;
                    }
              }
            bracketBlockStateStack->push_back(isBlockOpener);
            if (!isBlockOpener)
              {
                inStatementIndentStackSizeStack->push_back(inStatementIndentStack->size());
                registerInStatementIndent(line, i, spaceTabCount, 0, true);
                parenDepth++;
                if (i == 0)
                  shouldIndentBrackettedLine = false;

                continue;
              }

            // this bracket is a block opener...

            ++lineOpeningBlocksNum;

            if (isInClassHeader)
              isInClassHeader = false;
            if (isInClassHeaderTab)
              {
                isInClassHeaderTab = false;
                tabCount -= 2;
              }

            blockParenDepthStack->push_back(parenDepth);
            blockStatementStack->push_back(isInStatement);

            inStatementIndentStackSizeStack->push_back(inStatementIndentStack->size());

            blockTabCount += isInStatement? 1 : 0;
            parenDepth = 0;
            isInStatement = false;

            tempStacks->push_back(new vector<const string*>);
            headerStack->push_back(&AS_OPEN_BRACKET);
            lastLineHeader = &AS_OPEN_BRACKET; // <------

            continue;
          }

        //check if a header has been reached
        if (prevCh == ' ')
          {
            bool isIndentableHeader = true;
            const string *newHeader = findHeader(line, i, headers);
            if (newHeader != NULL)
              {
                // if we reached here, then this is a header...
                isInHeader = true;

                vector<const string*> *lastTempStack;
                if (tempStacks->empty())
                  lastTempStack = NULL;
                else
                  lastTempStack = tempStacks->back();

                // if a new block is opened, push a new stack into tempStacks to hold the
                // future list of headers in the new block.

                // take care of the special case: 'else if (...)'
                if (newHeader == &AS_IF && lastLineHeader == &AS_ELSE)
                  {
                    //spaceTabCount += indentLength; // to counter the opposite addition that occurs when the 'if' is registered below...
                    headerStack->pop_back();
                  }

                // take care of 'else'
                else if (newHeader == &AS_ELSE)
                  {
                    if (lastTempStack != NULL)
                      {
                        int indexOfIf = indexOf(*lastTempStack, &AS_IF); // <---
                        if (indexOfIf != -1)
                          {
                            // recreate the header list in headerStack up to the previous 'if'
                            // from the temporary snapshot stored in lastTempStack.
                            int restackSize = lastTempStack->size() - indexOfIf - 1;
                            for (int r=0; r<restackSize; r++)
                              {
                                headerStack->push_back(lastTempStack->back());
                                lastTempStack->pop_back();
                              }
                            if (!closingBracketReached)
                              tabCount += restackSize;
                          }
                        /*
                         * If the above if is not true, i.e. no 'if' before the 'else',
                         * then nothing beautiful will come out of this...
                         * I should think about inserting an Exception here to notify the caller of this...
                         */
                      }
                  }

                // check if 'while' closes a previous 'do'
                else if (newHeader == &AS_WHILE)
                  {
                    if (lastTempStack != NULL)
                      {
                        int indexOfDo = indexOf(*lastTempStack, &AS_DO); // <---
                        if (indexOfDo != -1)
                          {
                            // recreate the header list in headerStack up to the previous 'do'
                            // from the temporary snapshot stored in lastTempStack.
                            int restackSize = lastTempStack->size() - indexOfDo - 1;
                            for (int r=0; r<restackSize; r++)
                              {
                                headerStack->push_back(lastTempStack->back());
                                lastTempStack->pop_back();
                              }
                            if (!closingBracketReached)
                              tabCount += restackSize;
                          }
                      }
                  }
                // check if 'catch' closes a previous 'try' or 'catch'
                else if (newHeader == &AS_CATCH || newHeader == &AS_FINALLY)
                  {
                    if (lastTempStack != NULL)
                      {
                        int indexOfTry = indexOf(*lastTempStack, &AS_TRY);
                        if (indexOfTry == -1)
                          indexOfTry = indexOf(*lastTempStack, &AS_CATCH);
                        if (indexOfTry != -1)
                          {
                            // recreate the header list in headerStack up to the previous 'try'
                            // from the temporary snapshot stored in lastTempStack.
                            int restackSize = lastTempStack->size() - indexOfTry - 1;
                            for (int r=0; r<restackSize; r++)
                              {
                                headerStack->push_back(lastTempStack->back());
                                lastTempStack->pop_back();
                              }

                            if (!closingBracketReached)
                              tabCount += restackSize;
                          }
                      }
                  }
                else if (newHeader == &AS_CASE)
                  {
                    isInCase = true;
                    if (!caseIndent)
                      --tabCount;
                  }
                else if(newHeader == &AS_DEFAULT)
                  {
                    isInCase = true;
                    if (!caseIndent)
                      --tabCount;
                  }
                else if (newHeader == &AS_PUBLIC || newHeader == &AS_PROTECTED || newHeader == &AS_PRIVATE)
                  {
                    if (isCStyle && !isInClassHeader)
                      --tabCount;
                    isIndentableHeader = false;
                  }
                //else if ((newHeader == &STATIC || newHeader == &SYNCHRONIZED) &&
                //         !headerStack->empty() &&
                //         (headerStack->back() == &STATIC || headerStack->back() == &SYNCHRONIZED))
                //{
                //    isIndentableHeader = false;
                //}
                else if (newHeader == &AS_STATIC
                         || newHeader == &AS_SYNCHRONIZED
                         || (newHeader == &AS_CONST && isCStyle))
                  {
                    if (!headerStack->empty() &&
                        (headerStack->back() == &AS_STATIC
                         || headerStack->back() == &AS_SYNCHRONIZED
                         || headerStack->back() == &AS_CONST))
                      {
                        isIndentableHeader = false;
                      }
                    else
                      {
                        isIndentableHeader = false;
                        probationHeader = newHeader;
                      }
                  }
                else if (newHeader == &AS_CONST)
                  {
                    // this will be entered only if NOT in C style
                    // since otherwise the CONST would be found to be a probstion header...

                    //if (isCStyle)
                    //  isInConst = true;
                    isIndentableHeader = false;
                  }
                /*
                              else if (newHeader == &OPERATOR)
                              {
                                  if (isCStyle)
                                      isInOperator = true;
                                  isIndentableHeader = false;
                              }
                */
                else if (newHeader == &AS_TEMPLATE)
                  {
                    if (isCStyle)
                      isInTemplate = true;
                    isIndentableHeader = false;
                  }


                if (isIndentableHeader)
                  {
                    // 3.2.99
                    //spaceTabCount-=indentLength;
                    headerStack->push_back(newHeader);
                    isInStatement = false;
                    if (indexOf(nonParenHeaders, newHeader) == -1)
                      {
                        isInConditional = true;
                      }
                    lastLineHeader = newHeader;
                  }
                else
                  isInHeader = false;

                //lastLineHeader = newHeader;

                outBuffer.append(newHeader->substr(1));
                i += newHeader->length() - 1;

                continue;
              }
          }

        if (isCStyle && !isalpha(prevCh)
            && line.COMPARE(i, 8, AS_OPERATOR) == 0 && !isalnum(line[i+8]))
          {
            isInOperator = true;
            outBuffer.append(AS_OPERATOR.substr(1));
            i += 7;
            continue;
          }

        if (ch == '?')
          isInQuestion = true;


        // special handling of 'case' statements
        if (ch == ':')
          {
            if (line.length() > i+1 && line[i+1] == ':') // look for ::
              {
                ++i;
                outBuffer.append(1, ':');
                ch = ' ';
                continue;
              }

            else if (isCStyle && isInClass && prevNonSpaceCh != ')')
              {
              // BEGIN Content of ASBeautifier.cpp.BITFIELD.patch:
              
                unsigned int chIndex;
   			    char nextCh = 0;
                for (chIndex = i+1; chIndex < line.length(); chIndex++)
            		if (!isWhiteSpace(line[chIndex]))
						break;
					if (chIndex< line.length())
       					nextCh = line[chIndex];
				int nWord =0;
    			for (chIndex = 0; chIndex < i; chIndex++)
				{
					if (!isWhiteSpace(line[chIndex]))
					{
						nWord ++;
						while (!isWhiteSpace(line[++chIndex]));
					}									
				}
				if ((nextCh >= '0' && nextCh <= '9') || (nWord >1))
					continue;
              // END Content of ASBeautifier.cpp.BITFIELD.patch:
                
                --tabCount;
                // found a 'private:' or 'public:' inside a class definition
                // so do nothing special
              }

            else if (isCStyle && isInClassHeader)
              {

                // found a 'class A : public B' definition
                // so do nothing special
              }

            else if (isInQuestion)
              {
                isInQuestion = false;
              }
            else if (isCStyle && prevNonSpaceCh == ')')
              {
                isInClassHeader = true;
                if (i==0)
                  tabCount += 2;
              }
            else
              {
                currentNonSpaceCh = ';'; // so that brackets after the ':' will appear as block-openers
                if (isInCase)
                  {
                    isInCase = false;
                    ch = ';'; // from here on, treat char as ';'
                  } 
              // BEGIN content of ASBeautifier.cpp.BITFIELD.patch.bz2
              else // bitfield or labels
								{
				unsigned int chIndex;
				char nextCh = 0;
				for (chIndex = i+1; (isCStyle && chIndex < line.length()); chIndex++)
					if (!isWhiteSpace(line[chIndex]))
						break;
				if (chIndex< line.length())
					nextCh = line[chIndex];

     			int nWord =0;
 				for (chIndex = 0; chIndex < i; chIndex++)
				{
					if (!isWhiteSpace(line[chIndex]))
					{
						nWord ++;
						while (!isWhiteSpace(line[++chIndex]));
					}									
				}
         		if (isCStyle &&  (nextCh >= '0' && nextCh <= '9') || (nWord >1))
				{
					continue;
				}
                // END content of ASASBeautifier.cpp.BITFIELD.patch.bz2

                else // is in a label (e.g. 'label1:')
                  {
                    if (labelIndent)
                      --tabCount; // unindent label by one indent
                    else
                      tabCount = 0; // completely flush indent to left
                  }

              // BEGIN content of ASASBeautifier.cpp.BITFIELD.patch.bz2
                }
            // END content of ASASBeautifier.cpp.BITFIELD.patch.bz2

              }
          }

        if ((ch == ';'  || (parenDepth>0 && ch == ','))  && !inStatementIndentStackSizeStack->empty())
          while ((unsigned int)inStatementIndentStackSizeStack->back() + (parenDepth>0 ? 1 : 0)  < inStatementIndentStack->size())
            inStatementIndentStack->pop_back();


        // handle ends of statements
        if ( (ch == ';' && parenDepth == 0) || ch == '}'/* || (ch == ',' && parenDepth == 0)*/)
          {
            if (ch == '}')
              {
                // first check if this '}' closes a previous block, or a static array...
                if (!bracketBlockStateStack->empty())
                  {
                    bool bracketBlockState = bracketBlockStateStack->back();
                    bracketBlockStateStack->pop_back();
                    if (!bracketBlockState)
                      {
                        if (!inStatementIndentStackSizeStack->empty())
                          {
                            // this bracket is a static array

                            unsigned int previousIndentStackSize = inStatementIndentStackSizeStack->back();
                            inStatementIndentStackSizeStack->pop_back();
                            while (previousIndentStackSize < inStatementIndentStack->size())
                              inStatementIndentStack->pop_back();
                            parenDepth--;
                            if (i == 0)
                              shouldIndentBrackettedLine = false;

                            if (!parenIndentStack->empty())
                              {
                                int poppedIndent = parenIndentStack->back();
                                parenIndentStack->pop_back();
                                if (i == 0)
                                  spaceTabCount = poppedIndent;
                              }
                          }
                        continue;
                      }
                  }

                // this bracket is block closer...

                ++lineClosingBlocksNum;

                if(!inStatementIndentStackSizeStack->empty())
                  inStatementIndentStackSizeStack->pop_back();

                if (!blockParenDepthStack->empty())
                  {
                    parenDepth = blockParenDepthStack->back();
                    blockParenDepthStack->pop_back();
                    isInStatement = blockStatementStack->back();
                    blockStatementStack->pop_back();

                    if (isInStatement)
                      blockTabCount--;
                  }

                closingBracketReached = true;
                int headerPlace = indexOf(*headerStack, &AS_OPEN_BRACKET); // <---
                if (headerPlace != -1)
                  {
                    const string *popped = headerStack->back();
                    while (popped != &AS_OPEN_BRACKET)
                      {
                        headerStack->pop_back();
                        popped = headerStack->back();
                      }
                    headerStack->pop_back();

                    if (!tempStacks->empty())
                      {
                        vector<const string*> *temp =  tempStacks->back();
                        tempStacks->pop_back();
                        delete temp;
                      }
                  }


                ch = ' '; // needed due to cases such as '}else{', so that headers ('else' tn tih case) will be identified...
              }

            /*
             * Create a temporary snapshot of the current block's header-list in the
             * uppermost inner stack in tempStacks, and clear the headerStack up to
             * the begining of the block.
             * Thus, the next future statement will think it comes one indent past
             * the block's '{' unless it specifically checks for a companion-header
             * (such as a previous 'if' for an 'else' header) within the tempStacks,
             * and recreates the temporary snapshot by manipulating the tempStacks.
             */
            if (!tempStacks->back()->empty())
              while (!tempStacks->back()->empty())
                tempStacks->back()->pop_back();
            while (!headerStack->empty() && headerStack->back() != &AS_OPEN_BRACKET)
              {
                tempStacks->back()->push_back(headerStack->back());
                headerStack->pop_back();
              }

            if (parenDepth == 0 && ch == ';')
              isInStatement=false;

            isInClassHeader = false;

            continue;
          }


        // check for preBlockStatements ONLY if not within parenthesies
        // (otherwise 'struct XXX' statements would be wrongly interpreted...)
        if (prevCh == ' ' && !isInTemplate && parenDepth == 0)
          {
            const string *newHeader = findHeader(line, i, preBlockStatements);
            if (newHeader != NULL)
              {
                isInClassHeader = true;
                outBuffer.append(newHeader->substr(1));
                i += newHeader->length() - 1;
                //if (isCStyle)
                headerStack->push_back(newHeader);
              }
          }

        // Handle operators
        //

        ////        // PRECHECK if a '==' or '--' or '++' operator was reached.
        ////        // If not, then register an indent IF an assignment operator was reached.
        ////        // The precheck is important, so that statements such as 'i--==2' are not recognized
        ////        // to have assignment operators (here, '-=') in them . . .

        const string *foundAssignmentOp = NULL;
        const string *foundNonAssignmentOp = NULL;

        immediatelyPreviousAssignmentOp = NULL;

        // Check if an operator has been reached.
        foundAssignmentOp = findHeader(line, i, assignmentOperators, false);
        foundNonAssignmentOp = findHeader(line, i, nonAssignmentOperators, false);

        // Since findHeader's boundry checking was not used above, it is possible
        // that both an assignment op and a non-assignment op where found,
        // e.g. '>>' and '>>='. If this is the case, treat the LONGER one as the
        // found operator.
        if (foundAssignmentOp != NULL && foundNonAssignmentOp != NULL)
          if (foundAssignmentOp->length() < foundNonAssignmentOp->length())
            foundAssignmentOp = NULL;
          else
            foundNonAssignmentOp = NULL;

        if (foundNonAssignmentOp != NULL)
          {
            if (foundNonAssignmentOp->length() > 1)
              {
                outBuffer.append(foundNonAssignmentOp->substr(1));
                i += foundNonAssignmentOp->length() - 1;
              }
          }

        else if (foundAssignmentOp != NULL)

          {
            if (foundAssignmentOp->length() > 1)
              {
                outBuffer.append(foundAssignmentOp->substr(1));
                i += foundAssignmentOp->length() - 1;
              }

            if (!isInOperator && !isInTemplate)
              {
                registerInStatementIndent(line, i, spaceTabCount, 0, false);
                immediatelyPreviousAssignmentOp = foundAssignmentOp;
                isInStatement = true;
              }
          }

        /*
                immediatelyPreviousAssignmentOp = NULL;
                bool isNonAssingmentOperator = false;
                for (int n = 0; n < nonAssignmentOperators.size(); n++)
                    if (line.COMPARE(i, nonAssignmentOperators[n]->length(), *(nonAssignmentOperators[n])) == 0)
                    {
                        if (nonAssignmentOperators[n]->length() > 1)
                        {
                            outBuffer.append(nonAssignmentOperators[n]->substr(1));
                            i += nonAssignmentOperators[n]->length() - 1;
                        }
                        isNonAssingmentOperator = true;
                        break;
                    }
                if (!isNonAssingmentOperator)
                {
                    for (int a = 0; a < assignmentOperators.size(); a++)
                        if (line.COMPARE(i, assignmentOperators[a]->length(), *(assignmentOperators[a])) == 0)
                        {
                            if (assignmentOperators[a]->length() > 1)
                            {
                                outBuffer.append(assignmentOperators[a]->substr(1));
                                i += assignmentOperators[a]->length() - 1;
                            }
         
                            if (!isInOperator && !isInTemplate)
                            {
                                registerInStatementIndent(line, i, spaceTabCount, 0, false);
                                immediatelyPreviousAssignmentOp = assignmentOperators[a];
                                isInStatement = true;
                            }
                            break;
                        }
                }
        */

        if (isInOperator)
          isInOperator = false;
      }

    // handle special cases of unindentation:

    /*
     * if '{' doesn't follow an immediately previous '{' in the headerStack
     * (but rather another header such as "for" or "if", then unindent it
     * by one indentation relative to its block.
     */
    //    cerr << endl << lineOpeningBlocksNum << " " <<  lineClosingBlocksNum << " " <<  previousLastLineHeader << endl;

    // indent #define lines with one less tab
    //if (isInDefine)
    //    tabCount -= defineTabCount-1;


    if (!lineStartsInComment
        && !blockIndent
        && outBuffer.length()>0
        && outBuffer[0]=='{'
        && !(lineOpeningBlocksNum > 0 && lineOpeningBlocksNum == lineClosingBlocksNum)
        && !(headerStack->size() > 1 && (*headerStack)[headerStack->size()-2] == &AS_OPEN_BRACKET)
        && shouldIndentBrackettedLine)
      --tabCount;

    else if (!lineStartsInComment
             && outBuffer.length()>0
             && outBuffer[0]=='}'
             && shouldIndentBrackettedLine )
      --tabCount;

    // correctly indent one-line-blocks...
    else if (!lineStartsInComment
             && outBuffer.length()>0
             && lineOpeningBlocksNum > 0
             && lineOpeningBlocksNum == lineClosingBlocksNum
             && previousLastLineHeader != NULL
             && previousLastLineHeader != &AS_OPEN_BRACKET)
      tabCount -= 1; //lineOpeningBlocksNum - (blockIndent ? 1 : 0);

    if (tabCount < 0)
      tabCount = 0;

    // take care of extra bracket indentatation option...
    if (bracketIndent && outBuffer.length()>0 && shouldIndentBrackettedLine)
      if (outBuffer[0]=='{' || outBuffer[0]=='}')
        tabCount++;


    if (isInDefine)
      {
        if (outBuffer[0] == '#')
          {
            string preproc = trim(string(outBuffer.c_str() + 1));
            if (preproc.COMPARE(0, 6, string("define")) == 0)
              {
                if (!inStatementIndentStack->empty()
                    && inStatementIndentStack->back() > 0)
                  {
                    defineTabCount = tabCount;
                  }
                else
                  {
                    defineTabCount = tabCount - 1;
                    tabCount--;
                  }
              }
          }

        tabCount -= defineTabCount;
      }

    if (tabCount < 0)
      tabCount = 0;


    // finally, insert indentations into begining of line

    prevFinalLineSpaceTabCount = spaceTabCount;
    prevFinalLineTabCount = tabCount;

    if (shouldForceTabIndentation)
      {
        tabCount += spaceTabCount / indentLength;
        spaceTabCount = spaceTabCount % indentLength;
      }

    outBuffer = preLineWS(spaceTabCount,tabCount) + outBuffer;

    if (lastLineHeader != NULL)
      previousLastLineHeader = lastLineHeader;

    return outBuffer;
  }


  string ASBeautifier::preLineWS(int spaceTabCount, int tabCount)
  {
    string ws;

    for (int i=0; i<tabCount; i++)
      ws += indentString;

    while ((spaceTabCount--) > 0)
      ws += string(" ");

    return ws;

  }

  /**
   * register an in-statement indent.
   */
  void ASBeautifier::registerInStatementIndent(const string &line, int i, int spaceTabCount,
      int minIndent, bool updateParenStack)
  {
    int inStatementIndent;
    int remainingCharNum = line.length() - i;
    int nextNonWSChar = 1;

    nextNonWSChar = getNextProgramCharDistance(line, i);

    // if indent is around the last char in the line, indent instead 2 spaces from the previous indent
    if (nextNonWSChar == remainingCharNum)
      {
        int previousIndent = spaceTabCount;
        if (!inStatementIndentStack->empty())
          previousIndent = inStatementIndentStack->back();

        inStatementIndentStack->push_back(/*2*/ indentLength + previousIndent );
        if (updateParenStack)
          parenIndentStack->push_back( previousIndent );
        return;
      }

    if (updateParenStack)
      parenIndentStack->push_back(i+spaceTabCount);

    inStatementIndent = i + nextNonWSChar + spaceTabCount;

    if (i + nextNonWSChar < minIndent)
      inStatementIndent = minIndent + spaceTabCount;

    if (i + nextNonWSChar > maxInStatementIndent)
      inStatementIndent =  indentLength*2 + spaceTabCount;



    if (!inStatementIndentStack->empty() &&
        inStatementIndent < inStatementIndentStack->back())
      inStatementIndent = inStatementIndentStack->back();

    inStatementIndentStack->push_back(inStatementIndent);
  }

  /**
   * get distance to the next non-white sspace, non-comment character in the line.
   * if no such character exists, return the length remaining to the end of the line.
   */
  int ASBeautifier::getNextProgramCharDistance(const string &line, int i)
  {
    bool inComment = false;
    int remainingCharNum = line.length() - i;
    int charDistance = 1;
    int ch;

    for (charDistance = 1; charDistance < remainingCharNum; charDistance++)
      {
        ch = line[i + charDistance];
        if (inComment)
          {
            if (line.COMPARE(i + charDistance, 2, AS_CLOSE_COMMENT) == 0)
              {
                charDistance++;
                inComment = false;
              }
            continue;
          }
        else if (isWhiteSpace(ch))
          continue;
        else if (ch == '/')
          {
            if (line.COMPARE(i + charDistance, 2, AS_OPEN_LINE_COMMENT) == 0)
              return remainingCharNum;
            else if (line.COMPARE(i + charDistance, 2, AS_OPEN_COMMENT) == 0)
              {
                charDistance++;
                inComment = true;
              }
          }
        else
          return charDistance;
      }

    return charDistance;
  }


  /**
   * check if a specific character can be used in a legal variable/method/class name
   *
   * @return          legality of the char.
   * @param ch        the character to be checked.
   */
  bool ASBeautifier::isLegalNameChar(char ch) const
    {
      return (isalnum(ch) //(ch>='a' && ch<='z') || (ch>='A' && ch<='Z') || (ch>='0' && ch<='9') ||
              || ch=='.' || ch=='_' || (!isCStyle && ch=='$') || (isCStyle && ch=='~'));
    }


  /**
   * check if a specific line position contains a header, out of several possible headers.
   *
   * @return    a pointer to the found header. if no header was found then return NULL.
   */
  const string *ASBeautifier::findHeader(const string &line, int i, const vector<const string*> &possibleHeaders, bool checkBoundry)
  {
    int maxHeaders = possibleHeaders.size();
    const string *header = NULL;
    int p;

    for (p=0; p < maxHeaders; p++)
      {
        header = possibleHeaders[p];

        if (line.COMPARE(i, header->length(), *header) == 0)
          {
            // check that this is a header and not a part of a longer word
            // (e.g. not at its begining, not at its middle...)

            int lineLength = line.length();
            int headerEnd = i + header->length();
            char startCh = (*header)[0];   // first char of header
            char endCh = 0;                // char just after header
            char prevCh = 0;               // char just before header

            if (headerEnd < lineLength)
              {
                endCh = line[headerEnd];
              }
            if (i > 0)
              {
                prevCh = line[i-1];
              }

            if (!checkBoundry)
              {
                return header;
              }
            else if (prevCh != 0
                     && isLegalNameChar(startCh)
                     && isLegalNameChar(prevCh))
              {
                return NULL;
              }
            else if (headerEnd >= lineLength
                     || !isLegalNameChar(startCh)
                     || !isLegalNameChar(endCh))
              {
                return header;
              }
            else
              {
                return NULL;
              }
          }
      }

    return NULL;
  }


  /**
   * check if a specific character can be used in a legal variable/method/class name
   *
   * @return          legality of the char.
   * @param ch        the character to be checked.
   */
  bool ASBeautifier::isWhiteSpace(char ch) const
    {
      return (ch == ' ' || ch == '\t');
    }

  /**
   * find the index number of a string element in a container of strings
   *
   * @return              the index number of element in the ocntainer. -1 if element not found.
   * @param container     a vector of strings.
   * @param element       the element to find .
   */
  int ASBeautifier::indexOf(vector<const string*> &container, const string *element)
  {
    vector<const string*>::const_iterator where;

    where= find(container.begin(), container.end(), element);
    if (where == container.end())
      return -1;
    else
      return where - container.begin();
  }

  /**
   * trim removes the white space surrounding a line.
   *
   * @return          the trimmed line.
   * @param str       the line to trim.
   */
  string ASBeautifier::trim(const string &str)
  {

    int start = 0;
    int end = str.length() - 1;

    while (start < end && isWhiteSpace(str[start]))
      start++;

    while (start <= end && isWhiteSpace(str[end]))
      end--;

    string returnStr(str, start, end+1-start);
    return returnStr;
  }

#ifdef USES_NAMESPACE
}
#endif
/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * compiler_defines.h   (1 January 1999)
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C++, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 */


#ifndef ASBEAUTIFIER_H
#define ASBEAUTIFIER_H

#include "ASResource.h"
#include "compiler_defines.h"
#include "ASSourceIterator.h"

#include <string>
#include <vector>


using namespace std;

namespace astyle
  {

  enum BracketMode   { NONE_MODE, ATTACH_MODE, BREAK_MODE, BDAC_MODE };
  enum BracketType   { NULL_TYPE = 0,
                       DEFINITION_TYPE = 1,
                       COMMAND_TYPE = 2,
                       ARRAY_TYPE  = 4,
                       SINGLE_LINE_TYPE = 8};


  class ASBeautifier : protected ASResource
    {
    public:
      ASBeautifier();
      virtual ~ASBeautifier();
      virtual void init(ASSourceIterator* iter); // pointer to dynamically created iterator.
      virtual void init();
      virtual bool hasMoreLines() const;
      virtual string nextLine();
      virtual string beautify(const string &line);
      void setTabIndentation(int length = 4, bool forceTabs = false);
      void setSpaceIndentation(int length = 4);
      void setMaxInStatementIndentLength(int max);
      void setMinConditionalIndentLength(int min);
      void setClassIndent(bool state);
      void setSwitchIndent(bool state);
      void setCaseIndent(bool state);
      void setBracketIndent(bool state);
      void setBlockIndent(bool state);
      void setNamespaceIndent(bool state);
      void setLabelIndent(bool state);
      void setCStyle();
      void setJavaStyle();
      void setEmptyLineFill(bool state);
      void setPreprocessorIndent(bool state);


    protected:
      int getNextProgramCharDistance(const string &line, int i);
      bool isLegalNameChar(char ch) const;
      bool isWhiteSpace(char ch) const;
      const string *findHeader(const string &line, int i,
                               const vector<const string*> &possibleHeaders,
                               bool checkBoundry = true);
      string trim(const string &str);
      int indexOf(vector<const string*> &container, const string *element);

    private:
      ASBeautifier(const ASBeautifier &copy);
      void operator=(ASBeautifier&); // not to be implemented

      void initStatic();
      void registerInStatementIndent(const string &line, int i, int spaceTabCount,
                                     int minIndent, bool updateParenStack);
      string preLineWS(int spaceTabCount, int tabCount);

      static vector<const string*> headers;
      static vector<const string*> nonParenHeaders;
      static vector<const string*> preprocessorHeaders;
      static vector<const string*> preBlockStatements;
      static vector<const string*> assignmentOperators;
      static vector<const string*> nonAssignmentOperators;

      static bool calledInitStatic;

      ASSourceIterator *sourceIterator;
      vector<ASBeautifier*> *waitingBeautifierStack;
      vector<ASBeautifier*> *activeBeautifierStack;
      vector<int> *waitingBeautifierStackLengthStack;
      vector<int> *activeBeautifierStackLengthStack;
      vector<const string*> *headerStack;
      vector< vector<const string*>* > *tempStacks;
      vector<int> *blockParenDepthStack;
      vector<bool> *blockStatementStack;
      vector<bool> *parenStatementStack;
      vector<int> *inStatementIndentStack;
      vector<int> *inStatementIndentStackSizeStack;
      vector<int> *parenIndentStack;
      vector<bool> *bracketBlockStateStack;
      string indentString;
      const string *currentHeader;
      const string *previousLastLineHeader;
      const string *immediatelyPreviousAssignmentOp;
      const string *probationHeader;
      bool isInQuote;
      bool isInComment;
      bool isInCase;
      bool isInQuestion;
      bool isInStatement;
      bool isInHeader;
      bool isCStyle;
      bool isInOperator;
      bool isInTemplate;
      bool isInConst;
      bool isInDefine;
      bool isInDefineDefinition;
      bool classIndent;
      bool isInClassHeader;
      bool isInClassHeaderTab;
      bool switchIndent;
      bool caseIndent;
      bool namespaceIndent;
      bool bracketIndent;
      bool blockIndent;
      bool labelIndent;
      bool preprocessorIndent;
      bool isInConditional;
      bool isMinimalConditinalIndentSet;
      bool shouldForceTabIndentation;
      int minConditionalIndent;
      int parenDepth;
      int indentLength;
      int blockTabCount;
      unsigned int leadingWhiteSpaces;
      int maxInStatementIndent;
      int templateDepth;
      char quoteChar;
      char prevNonSpaceCh;
      char currentNonSpaceCh;
      char currentNonLegalCh;
      char prevNonLegalCh;
      int prevFinalLineSpaceTabCount;
      int prevFinalLineTabCount;
      bool emptyLineFill;
      bool backslashEndsPrevLine;
      int defineTabCount;
    };
}

#endif
/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * ASFormatter.cpp
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C++, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 *
 *
 * Patches:
 * 26 November 1998 - Richard Bullington -
 *        A correction of line-breaking in headers following '}',
 
 *        was created using a variation of a  patch by Richard Bullington.
 * 08 May 2004
 *        applied   ASFormatter450670.patch.bz2, ASFormatter.cpp.patch.bz2,
 *                  patch1_ssvb_patch.tar.gz
 */

#include "compiler_defines.h"
#include "ASFormatter.h"


#include <string>
#include <cctype>
#include <vector>
#include <algorithm>
#include <iostream>


#define INIT_CONTAINER(container, value)     {if ( (container) != NULL ) delete (container); (container) = (value); }
#define DELETE_CONTAINER(container)          {if ( (container) != NULL ) delete (container) ; }
#define IS_A(a,b)                            ( ((a) & (b)) == (b))
#ifdef USES_NAMESPACE
using namespace std;

namespace astyle
  {
#endif


  bool ASFormatter::calledInitStatic = false;
  vector<const string*> ASFormatter::headers;
  vector<const string*> ASFormatter::nonParenHeaders;
  vector<const string*> ASFormatter::preprocessorHeaders;
  vector<const string*> ASFormatter::preDefinitionHeaders;
  vector<const string*> ASFormatter::preCommandHeaders;
  vector<const string*> ASFormatter::operators;
  vector<const string*> ASFormatter::assignmentOperators;


  /**
   * Constructor of ASFormatter
   */
  ASFormatter::ASFormatter()
  {
    staticInit();

    preBracketHeaderStack = NULL;
    bracketTypeStack = NULL;
    parenStack = NULL;

    sourceIterator = NULL;
    bracketFormatMode = NONE_MODE;
    shouldPadOperators = false;
    shouldPadParenthesies = false;
    shouldBreakOneLineBlocks = true;
    shouldBreakOneLineStatements = true;
    shouldConvertTabs = false;
    shouldBreakBlocks = false;
    shouldBreakClosingHeaderBlocks = false;
    shouldBreakClosingHeaderBrackets = false;
    shouldBreakElseIfs = false;
  }

  /**
   * Destructor of ASFormatter
   */
  ASFormatter::~ASFormatter()
  {
    DELETE_CONTAINER( preBracketHeaderStack );
  }

  /**
   * initialization of static data of ASFormatter.
   */
  void ASFormatter::staticInit()
  {
    if (calledInitStatic)
      return;

    calledInitStatic = true;

    headers.push_back(&AS_IF);
    headers.push_back(&AS_ELSE);
    headers.push_back(&AS_DO);
    headers.push_back(&AS_WHILE);
    headers.push_back(&AS_FOR);
    headers.push_back(&AS_SYNCHRONIZED);
    headers.push_back(&AS_TRY);
    headers.push_back(&AS_CATCH);
    headers.push_back(&AS_FINALLY);
    headers.push_back(&AS_SWITCH);
    headers.push_back(&AS_TEMPLATE);
    headers.push_back(&AS_FOREACH);
    headers.push_back(&AS_LOCK);
    headers.push_back(&AS_UNSAFE);
    headers.push_back(&AS_FIXED);
    headers.push_back(&AS_GET);
    headers.push_back(&AS_SET);
    headers.push_back(&AS_ADD);
    headers.push_back(&AS_REMOVE);

    nonParenHeaders.push_back(&AS_ELSE);
    nonParenHeaders.push_back(&AS_DO);
    nonParenHeaders.push_back(&AS_TRY);
    nonParenHeaders.push_back(&AS_FINALLY);
    nonParenHeaders.push_back(&AS_UNSAFE);
    nonParenHeaders.push_back(&AS_GET);
    nonParenHeaders.push_back(&AS_SET);
    nonParenHeaders.push_back(&AS_ADD);
    nonParenHeaders.push_back(&AS_REMOVE);

    //    nonParenHeaders.push_back(&AS_TEMPLATE);

    preDefinitionHeaders.push_back(&AS_CLASS);
    preDefinitionHeaders.push_back(&AS_INTERFACE);
    preDefinitionHeaders.push_back(&AS_NAMESPACE);
    preDefinitionHeaders.push_back(&AS_STRUCT);

    preCommandHeaders.push_back(&AS_EXTERN);
    preCommandHeaders.push_back(&AS_THROWS);
    preCommandHeaders.push_back(&AS_CONST);

    preprocessorHeaders.push_back(&AS_BAR_DEFINE);
    //// DEVEL: removed the folowing lines
    ////preprocessorHeaders.push_back(&AS_BAR_INCLUDE);
    ////preprocessorHeaders.push_back(&AS_BAR_IF); // #if or #ifdef
    ////preprocessorHeaders.push_back(&AS_BAR_EL); // #else or #elif
    ////preprocessorHeaders.push_back(&AS_BAR_ENDIF);

    operators.push_back(&AS_PLUS_ASSIGN);
    operators.push_back(&AS_MINUS_ASSIGN);
    operators.push_back(&AS_MULT_ASSIGN);
    operators.push_back(&AS_DIV_ASSIGN);
    operators.push_back(&AS_MOD_ASSIGN);
    operators.push_back(&AS_OR_ASSIGN);
    operators.push_back(&AS_AND_ASSIGN);
    operators.push_back(&AS_XOR_ASSIGN);
    operators.push_back(&AS_EQUAL);
    operators.push_back(&AS_PLUS_PLUS);
    operators.push_back(&AS_MINUS_MINUS);
    operators.push_back(&AS_NOT_EQUAL);
    operators.push_back(&AS_GR_EQUAL);
    operators.push_back(&AS_GR_GR_GR_ASSIGN);
    operators.push_back(&AS_GR_GR_ASSIGN);
    operators.push_back(&AS_GR_GR_GR);
    operators.push_back(&AS_GR_GR);
    operators.push_back(&AS_LS_EQUAL);
    operators.push_back(&AS_LS_LS_LS_ASSIGN);
    operators.push_back(&AS_LS_LS_ASSIGN);
    operators.push_back(&AS_LS_LS_LS);
    operators.push_back(&AS_LS_LS);
    operators.push_back(&AS_ARROW);
    operators.push_back(&AS_AND);
    operators.push_back(&AS_OR);
    operators.push_back(&AS_COLON_COLON);

    //// BUGFIX: removed the folowing lines
    ////    operators.push_back(&AS_PAREN_PAREN);
    ////    operators.push_back(&AS_BLPAREN_BLPAREN);

    operators.push_back(&AS_PLUS);
    operators.push_back(&AS_MINUS);
    operators.push_back(&AS_MULT);
    operators.push_back(&AS_DIV);
    operators.push_back(&AS_MOD);
    operators.push_back(&AS_QUESTION);
    operators.push_back(&AS_COLON);
    operators.push_back(&AS_ASSIGN);
    operators.push_back(&AS_LS);
    operators.push_back(&AS_GR);
    operators.push_back(&AS_NOT);
    operators.push_back(&AS_BIT_OR);
    operators.push_back(&AS_BIT_AND);
    operators.push_back(&AS_BIT_NOT);
    operators.push_back(&AS_BIT_XOR);
    operators.push_back(&AS_OPERATOR);
    operators.push_back(&AS_COMMA);
    //BEGIN Content Patch patch1_ssvb_patch.tar.gz
    operators.push_back(&AS_SEMICOLON);
    //END Content Patch patch1_ssvb_patch.tar.gz
    operators.push_back(&AS_RETURN);

    assignmentOperators.push_back(&AS_PLUS_ASSIGN);
    assignmentOperators.push_back(&AS_MINUS_ASSIGN);
    assignmentOperators.push_back(&AS_MULT_ASSIGN);
    assignmentOperators.push_back(&AS_DIV_ASSIGN);
    assignmentOperators.push_back(&AS_MOD_ASSIGN);
    assignmentOperators.push_back(&AS_XOR_ASSIGN);
    assignmentOperators.push_back(&AS_OR_ASSIGN);
    assignmentOperators.push_back(&AS_AND_ASSIGN);
    assignmentOperators.push_back(&AS_GR_GR_GR_ASSIGN);
    assignmentOperators.push_back(&AS_LS_LS_LS_ASSIGN);
    assignmentOperators.push_back(&AS_ASSIGN);
  }

  /**
   * initialize the ASFormatter.
   *
   * init() should be called every time a ASFormatter object is to start
   * formatting a NEW source file.
   * init() recieves a pointer to a DYNAMICALLY CREATED ASSourceIterator object
   * that will be used to iterate through the source code. This object will be
   * deleted during the ASFormatter's destruction, and thus should not be
   * deleted elsewhere.
   *
   * @param iter     a pointer to the DYNAMICALLY CREATED ASSourceIterator object.
   */
  void ASFormatter::init(ASSourceIterator *si)
  {
    ASBeautifier::init(si);
    sourceIterator = si;

    INIT_CONTAINER( preBracketHeaderStack, new vector<const string*> );
    INIT_CONTAINER( bracketTypeStack, new vector<BracketType> );
    bracketTypeStack->push_back(DEFINITION_TYPE);
    INIT_CONTAINER( parenStack, new vector<int> );
    parenStack->push_back(0);

    currentHeader = NULL;
    currentLine = string("");
    formattedLine = "";
    currentChar = ' ';
    previousCommandChar = ' ';
    previousNonWSChar = ' ';
    quoteChar = '"';
    charNum = 0;
    previousOperator = NULL;

    isVirgin = true;
    isInLineComment = false;
    isInComment = false;
    isInPreprocessor = false;
    doesLineStartComment = false;
    isInQuote = false;
    isSpecialChar = false;
    isNonParenHeader = true;
    foundPreDefinitionHeader = false;
    foundPreCommandHeader = false;
    foundQuestionMark = false;
    isInLineBreak = false;
    endOfCodeReached = false;
    isLineReady = false;
    isPreviousBracketBlockRelated = true;
    isInPotentialCalculation = false;
    //foundOneLineBlock = false;
    shouldReparseCurrentChar = false;
    passedSemicolon = false;
    passedColon = false;
    isInTemplate = false;
    shouldBreakLineAfterComments = false;
    isImmediatelyPostComment = false;
    isImmediatelyPostLineComment = false;
    isImmediatelyPostEmptyBlock = false;

    isPrependPostBlockEmptyLineRequested = false;
    isAppendPostBlockEmptyLineRequested = false;
    prependEmptyLine = false;

    foundClosingHeader = false;
    previousReadyFormattedLineLength = 0;

    isImmediatelyPostHeader = false;
    isInHeader = false;
  }

  /**
   * get the next formatted line.
   *
   * @return    formatted line.
   */

  string ASFormatter::nextLine()
  {
    const string *newHeader;
    bool isCharImmediatelyPostComment = false;
    bool isPreviousCharPostComment = false;
    bool isCharImmediatelyPostLineComment = false;
    bool isInVirginLine = isVirgin;
    bool isCharImmediatelyPostOpenBlock = false;
    bool isCharImmediatelyPostCloseBlock = false;
    bool isCharImmediatelyPostTemplate = false;
    bool isCharImmediatelyPostHeader = false;

    if (!isFormattingEnabled())
      return ASBeautifier::nextLine();

    while (!isLineReady)
      {
        if (shouldReparseCurrentChar)
          shouldReparseCurrentChar = false;
        else if (!getNextChar())
          {
            breakLine();
            return beautify(readyFormattedLine);
          }
        else // stuff to do when reading a new character...
          {
            // make sure that a virgin '{' at the begining ofthe file will be treated as a block...
            if (isInVirginLine && currentChar == '{')
              previousCommandChar = '{';
            isPreviousCharPostComment = isCharImmediatelyPostComment;
            isCharImmediatelyPostComment = false;
            isCharImmediatelyPostTemplate = false;
            isCharImmediatelyPostHeader = false;
          }

        if (isInLineComment)
          {
            appendCurrentChar();

            // explicitely break a line when a line comment's end is found.
            if (/*bracketFormatMode == ATTACH_MODE &&*/ charNum+1 == currentLine.length())
              {
                isInLineBreak = true;
                isInLineComment = false;
                isImmediatelyPostLineComment = true;
                currentChar = 0;  //make sure it is a neutral char.
              }
            continue;
          }
        else if (isInComment)
          {
            if (isSequenceReached(AS_CLOSE_COMMENT))
              {
                isInComment = false;
                isImmediatelyPostComment = true;
                appendSequence(AS_CLOSE_COMMENT);
                goForward(1);
              }
            else
              appendCurrentChar();

            continue;
          }

        // not in line comment or comment

        else if (isInQuote)
          {
            if (isSpecialChar)
              {
                isSpecialChar = false;
                appendCurrentChar();
              }
            else if (currentChar == '\\')
              {
                isSpecialChar = true;
                appendCurrentChar();
              }
            else if (quoteChar == currentChar)
              {
                isInQuote = false;
                appendCurrentChar();
              }
            else
              {
                appendCurrentChar();
              }

            continue;
          }



        // handle white space - needed to simplify the rest.
        if (isWhiteSpace(currentChar) || isInPreprocessor)
          {
            ////// DEVEL: if (isLegalNameChar(previousChar) && isLegalNameChar(peekNextChar()))
            appendCurrentChar();
            continue;
          }

        /* not in MIDDLE of quote or comment or white-space of any type ... */

        if (isSequenceReached(AS_OPEN_LINE_COMMENT))
          {
            isInLineComment = true;
            if (shouldPadOperators)
              appendSpacePad();
            appendSequence(AS_OPEN_LINE_COMMENT);
            goForward(1);
            continue;
          }
        else if (isSequenceReached(AS_OPEN_COMMENT))
          {
            isInComment = true;
            if (shouldPadOperators)
              appendSpacePad();
            appendSequence(AS_OPEN_COMMENT);
            goForward(1);
            continue;
          }
        else if (currentChar == '"' || currentChar == '\'')
          {
            isInQuote = true;
            quoteChar = currentChar;
            ////            if (shouldPadOperators)  // BUGFIX: these two lines removed. seem to be unneeded, and interfere with L"
            ////                appendSpacePad();    // BUFFIX:	TODO make sure the removal of these lines doesn't reopen old bugs...
            appendCurrentChar();
            continue;
          }

        /* not in quote or comment or white-space of any type ... */


        // check if in preprocessor
        // ** isInPreprocessor will be automatically reset at the begining
        //    of a new line in getnextChar()
        if (currentChar == '#')
          isInPreprocessor = true;

        if (isInPreprocessor)
          {
            appendCurrentChar();
            continue;
          }

        /* not in preprocessor ... */

        if (isImmediatelyPostComment)
          {
            isImmediatelyPostComment = false;
            isCharImmediatelyPostComment = true;
          }

        if (isImmediatelyPostLineComment)
          {
            isImmediatelyPostLineComment = false;
            isCharImmediatelyPostLineComment = true;
          }

        if (shouldBreakLineAfterComments)
          {
            shouldBreakLineAfterComments = false;
            shouldReparseCurrentChar = true;
            breakLine();
            continue;
          }

        // reset isImmediatelyPostHeader information
        if (isImmediatelyPostHeader)
          {
            isImmediatelyPostHeader = false;
            isCharImmediatelyPostHeader = true;

            // Make sure headers are broken from their succeeding blocks
            // (e.g.
            //     if (isFoo) DoBar();
            //  should become
            //     if (isFoo)
            //         DoBar;
            // )
            // But treat else if() as a special case which should not be broken!
            if (shouldBreakOneLineStatements)
              {
                // if may break 'else if()'s, ythen simply break the line

                if (shouldBreakElseIfs)
                  isInLineBreak = true;

                else
                  {
                    // make sure 'else if()'s are not broken.

                    bool isInElseIf = false;
                    const string *upcomingHeader;

                    upcomingHeader = findHeader(headers);
                    if (currentHeader == &AS_ELSE && upcomingHeader == &AS_IF)
                      isInElseIf = true;

                    if (!isInElseIf)
                      isInLineBreak = true;  ////BUGFIX: SHOULD NOT BE breakLine() !!!
                  }
              }
          }

        if (passedSemicolon)
          {
            passedSemicolon = false;
            if (parenStack->back() == 0)
              {
                shouldReparseCurrentChar = true;
                isInLineBreak = true;
                continue;
              }
          }

        if (passedColon)
          {
            passedColon = false;
            if (parenStack->back() == 0)
              {
                shouldReparseCurrentChar = true;
                isInLineBreak = true;
                continue;
              }
          }

        // Check if in template declaration, e.g. foo<bar> or foo<bar,fig>
        // If so, set isInTemplate to true
        //
        if (!isInTemplate && currentChar == '<')
          {
            int templateDepth = 0;
            const string *oper;
            for (unsigned int i=charNum;
                 i< currentLine.length();
                 i += (oper ? oper->length() : 1) )
              {
                oper = ASBeautifier::findHeader(currentLine, i, operators);

                if (oper == &AS_LS)
                  {
                    templateDepth++;
                  }
                else if (oper == &AS_GR)
                  {
                    templateDepth--;
                    if (templateDepth == 0)
                      {
                        // this is a template!
                        //
                        isInTemplate = true;
                        break;
                      }
                  }
                else if (oper == &AS_COMMA               // comma,     e.g. A<int, char>
                         || oper == &AS_BIT_AND       // reference, e.g. A<int&>
                         || oper == &AS_MULT          // pointer,   e.g. A<int*>
                         || oper == &AS_COLON_COLON)  // ::,        e.g. std::string
                  {
                    continue;
                  }
                else if (!isLegalNameChar(currentLine[i]) && !isWhiteSpace(currentLine[i]))
                  {
                    // this is not a template -> leave...
                    //
                    isInTemplate = false;
                    break;
                  }
              }
          }


        // handle parenthesies
        //
        if (currentChar == '(' || currentChar == '[' || (isInTemplate && currentChar == '<'))
          {
            parenStack->back()++;
          }
        else if (currentChar == ')' || currentChar == ']' || (isInTemplate && currentChar == '>'))
          {
            parenStack->back()--;
            if (isInTemplate && parenStack->back() == 0)
              {
                isInTemplate = false;
                isCharImmediatelyPostTemplate = true;
              }

            // check if this parenthesis closes a header, e.g. if (...), while (...)
            //
            if (isInHeader && parenStack->back() == 0)
              {
                isInHeader = false;
                isImmediatelyPostHeader = true;
              }

          }

        // handle brackets
        //
        BracketType bracketType = NULL_TYPE;

        if (currentChar == '{')
          {
            bracketType = getBracketType();
            foundPreDefinitionHeader = false;
            foundPreCommandHeader = false;

            bracketTypeStack->push_back(bracketType);
            preBracketHeaderStack->push_back(currentHeader);
            currentHeader = NULL;

            isPreviousBracketBlockRelated = !IS_A(bracketType, ARRAY_TYPE);
          }
        else if (currentChar == '}')
          {
            // if a request has been made to append a post block empty line,
            // but the block exists immediately before a closing bracket,
            // then there is not need for the post block empty line.
            //
            isAppendPostBlockEmptyLineRequested = false;

            if (!bracketTypeStack->empty())
              {
                bracketType = bracketTypeStack->back();
                bracketTypeStack->pop_back();

                isPreviousBracketBlockRelated = !IS_A(bracketType, ARRAY_TYPE);
              }

            if (!preBracketHeaderStack->empty())
              {
                currentHeader = preBracketHeaderStack->back();
                preBracketHeaderStack->pop_back();
              }
            else
              currentHeader = NULL;
          }

        if (!IS_A(bracketType, ARRAY_TYPE))
          {

            if (currentChar == '{')
              {
                parenStack->push_back(0);
              }
            else if (currentChar == '}')
              {
                if (!parenStack->empty())
                  {
                    parenStack->pop_back();
                  }
              }

            if (bracketFormatMode != NONE_MODE)
              {
                if (currentChar == '{')
                  {
                    if ( ( bracketFormatMode == ATTACH_MODE
                           || bracketFormatMode == BDAC_MODE && bracketTypeStack->size()>=2
                           && IS_A((*bracketTypeStack)[bracketTypeStack->size()-2], COMMAND_TYPE) /*&& isInLineBreak*/)
                         && !isCharImmediatelyPostLineComment )
                      {
                        appendSpacePad();
                        if (!isCharImmediatelyPostComment // do not attach '{' to lines that end with /**/ comments.
                            && previousCommandChar != '{'
                            && previousCommandChar != '}'
                            && previousCommandChar != ';') // '}' , ';' chars added for proper handling of '{' immediately after a '}' or ';'
                          appendCurrentChar(false);
                        else
                          appendCurrentChar(true);
                        continue;
                      }
                    else if (bracketFormatMode == BREAK_MODE
                             || bracketFormatMode == BDAC_MODE && bracketTypeStack->size()>=2
                             && IS_A((*bracketTypeStack)[bracketTypeStack->size()-2], DEFINITION_TYPE))
                      {
                        if ( shouldBreakOneLineBlocks || !IS_A(bracketType,  SINGLE_LINE_TYPE) )
                          breakLine();
                        appendCurrentChar();
                        continue;
                      }
                  }
                else if (currentChar == '}')
                  {
                    // bool origLineBreak = isInLineBreak;

                    // mark state of immediately after empty block
                    // this state will be used for locating brackets that appear immedately AFTER an empty block (e.g. '{} \n}').
                    if (previousCommandChar == '{')
                      isImmediatelyPostEmptyBlock = true;

                    if ( (!(previousCommandChar == '{' && isPreviousBracketBlockRelated) )          // this '{' does not close an empty block
                         && (shouldBreakOneLineBlocks || !IS_A(bracketType,  SINGLE_LINE_TYPE))  // astyle is allowed to break on line blocks
                         && !isImmediatelyPostEmptyBlock)                                        // this '}' does not immediately follow an empty block
                      {
                        breakLine();
                        appendCurrentChar();
                      }
                    else
                      {                       
                  // Content Patch    ASFormatter.cpp.patch.bz2
                      // if (!isCharImmediatelyPostComment)
                      if (!isCharImmediatelyPostComment && 
                  		!isCharImmediatelyPostLineComment)
                          isInLineBreak = false;
                        appendCurrentChar();
                        if (shouldBreakOneLineBlocks || !IS_A(bracketType,  SINGLE_LINE_TYPE))
                          shouldBreakLineAfterComments = true;
                      }

                    if (shouldBreakBlocks)
                      {
                        isAppendPostBlockEmptyLineRequested =true;
                      }

                    continue;
                  }
              }
          }

        if ( ( (previousCommandChar == '{'
                && isPreviousBracketBlockRelated)

               || (previousCommandChar == '}'
                   && !isImmediatelyPostEmptyBlock   // <--
                   && isPreviousBracketBlockRelated
                   && !isPreviousCharPostComment    // <-- Fixes wrongly appended newlines after '}' immediately after comments... 10/9/1999
                   && peekNextChar() != ' '))

             &&  (shouldBreakOneLineBlocks || !IS_A(bracketTypeStack->back(),  SINGLE_LINE_TYPE)) )
          {
            isCharImmediatelyPostOpenBlock = (previousCommandChar == '{');
            isCharImmediatelyPostCloseBlock = (previousCommandChar == '}');

            previousCommandChar = ' ';
            isInLineBreak = true;  //<----
          }

        // reset block handling flags
        isImmediatelyPostEmptyBlock = false;

        // look for headers
        if (!isInTemplate)
          {
            if ( (newHeader = findHeader(headers)) != NULL)
              {
                foundClosingHeader = false;
                const string *previousHeader;

                // recognize closing headers of do..while, if..else, try..catch..finally
                if ( (newHeader == &AS_ELSE && currentHeader == &AS_IF)
                     || (newHeader == &AS_WHILE && currentHeader == &AS_DO)
                     || (newHeader == &AS_CATCH && currentHeader == &AS_TRY)
                     || (newHeader == &AS_CATCH && currentHeader == &AS_CATCH)
                     || (newHeader == &AS_FINALLY && currentHeader == &AS_TRY)
                     || (newHeader == &AS_FINALLY && currentHeader == &AS_CATCH) )
                  foundClosingHeader = true;

                previousHeader = currentHeader;
                currentHeader = newHeader;

                // If in ATTACH or LINUX bracket modes, attach closing headers (e.g. 'else', 'catch')
                // to their preceding bracket,
                // But do not perform the attachment if the shouldBreakClosingHeaderBrackets is set!
                if (!shouldBreakClosingHeaderBrackets && foundClosingHeader && (bracketFormatMode == ATTACH_MODE || bracketFormatMode == BDAC_MODE) && previousNonWSChar == '}')
                  {
                    isInLineBreak = false;
                    appendSpacePad();

                    if (shouldBreakBlocks)
                      isAppendPostBlockEmptyLineRequested = false;
                  }

                //Check if a template definition as been reached, e.g. template<class A>
                if (newHeader == &AS_TEMPLATE)
                  {
                    isInTemplate = true;
                  }

                // check if the found header is non-paren header
                isNonParenHeader = ( find(nonParenHeaders.begin(), nonParenHeaders.end(),
                                          newHeader) != nonParenHeaders.end() );
                appendSequence(*currentHeader);
                goForward(currentHeader->length() - 1);
                // if padding is on, and a paren-header is found
                // then add a space pad after it.
                if (shouldPadOperators && !isNonParenHeader)
                  appendSpacePad();


                // Signal that a header has been reached
                // *** But treat a closing while() (as in do...while)
                //     as if it where NOT a header since a closing while()
                //     should never have a block after it!
                if (!(foundClosingHeader && currentHeader == &AS_WHILE))
                  {
                    isInHeader = true;
                    if (isNonParenHeader)
                      {
                        isImmediatelyPostHeader = true;
                        isInHeader = false;
                      }
                  }

                if (currentHeader == &AS_IF && previousHeader == &AS_ELSE)
                  isInLineBreak = false;

                if (shouldBreakBlocks)
                  {
                    if (previousHeader == NULL
                        && !foundClosingHeader
                        && !isCharImmediatelyPostOpenBlock)
                      {
                        isPrependPostBlockEmptyLineRequested = true;
                      }

                    if (currentHeader == &AS_ELSE
                        || currentHeader == &AS_CATCH
                        || currentHeader == &AS_FINALLY
                        || foundClosingHeader)
                      {
                        isPrependPostBlockEmptyLineRequested = false;
                      }

                    if (shouldBreakClosingHeaderBlocks
                        &&  isCharImmediatelyPostCloseBlock)
                      {
                        isPrependPostBlockEmptyLineRequested = true;
                      }

                  }

                continue;
              }
            else if ( (newHeader = findHeader(preDefinitionHeaders)) != NULL)
              {
                foundPreDefinitionHeader = true;
                appendSequence(*newHeader);
                goForward(newHeader->length() - 1);

                if (shouldBreakBlocks)
                  isPrependPostBlockEmptyLineRequested = true;

                continue;
              }
            else if ( (newHeader = findHeader(preCommandHeaders)) != NULL)
              {
                foundPreCommandHeader = true;
                appendSequence(*newHeader);
                goForward(newHeader->length() - 1);

                continue;
              }
          }

        if (previousNonWSChar == '}' || currentChar == ';')
          {
            if (shouldBreakOneLineStatements && currentChar == ';'
                && (shouldBreakOneLineBlocks || !IS_A(bracketTypeStack->back(),  SINGLE_LINE_TYPE)))
              {
                passedSemicolon = true;
              }

            if (shouldBreakBlocks && currentHeader != NULL && parenStack->back() == 0)
              {
                isAppendPostBlockEmptyLineRequested = true;
              }

            if (currentChar != ';')
              currentHeader = NULL; //DEVEL: is this ok?

            foundQuestionMark = false;
            foundPreDefinitionHeader = false;
            foundPreCommandHeader = false;
            isInPotentialCalculation = false;

          }

        if (currentChar == ':'
            && shouldBreakOneLineStatements
            && !foundQuestionMark // not in a ... ? ... : ... sequence
            && !foundPreDefinitionHeader // not in a definition block (e.g. class foo : public bar
            && previousCommandChar != ')' // not immediately after closing paren of a method header, e.g. ASFormatter::ASFormatter(...) : ASBeautifier(...)
            && previousChar != ':' // not part of '::'
            && peekNextChar() != ':') // not part of '::'
          {
            passedColon = true;
            if (shouldBreakBlocks)
              isPrependPostBlockEmptyLineRequested = true;
          }

        if (currentChar == '?')
          foundQuestionMark = true;

        if (shouldPadOperators)
          {
            if ((newHeader = findHeader(operators)) != NULL)
              {
                bool shouldPad = (newHeader != &AS_COLON_COLON
                                  && newHeader != &AS_PAREN_PAREN
                                  && newHeader != &AS_BLPAREN_BLPAREN
                                  && newHeader != &AS_PLUS_PLUS
                                  && newHeader != &AS_MINUS_MINUS
                                  && newHeader != &AS_NOT
                                  && newHeader != &AS_BIT_NOT
                                  && newHeader != &AS_ARROW
                                  && newHeader != &AS_OPERATOR
                                  && !(newHeader == &AS_MINUS && isInExponent())
                                  && !(newHeader == &AS_PLUS && isInExponent())
                                  && previousOperator != &AS_OPERATOR
                                  && !((newHeader == &AS_MULT || newHeader == &AS_BIT_AND)
                                       && isPointerOrReference())
                                  && !( (isInTemplate || isCharImmediatelyPostTemplate)
                                        && (newHeader == &AS_LS || newHeader == &AS_GR))
                                 );

                if (!isInPotentialCalculation)
                  if (find(assignmentOperators.begin(), assignmentOperators.end(), newHeader)
                      != assignmentOperators.end())
                    isInPotentialCalculation = true;

                // pad before operator
                if (shouldPad
                    && !(newHeader == &AS_COLON && !foundQuestionMark)
                    && newHeader != &AS_SEMICOLON
                    && newHeader != &AS_COMMA)
                  appendSpacePad();
                appendSequence(*newHeader);
                goForward(newHeader->length() - 1);

                // since this block handles '()' and '[]',
                // the parenStack must be updated here accordingly!
                if (newHeader == &AS_PAREN_PAREN
                    || newHeader == &AS_BLPAREN_BLPAREN)
                  parenStack->back()--;

                currentChar = (*newHeader)[newHeader->length() - 1];
                // pad after operator
                // but do not pad after a '-' that is a urinary-minus.
                if ( shouldPad && !(newHeader == &AS_MINUS && isUrinaryMinus()) )
                  appendSpacePad();

                previousOperator = newHeader;
                continue;
              }
          }
        //BEGIN Content Patch patch1_ssvb_patch.tar.gz
        if (currentChar == '(' || currentChar == '[' ) 
            isInPotentialCalculation = true;
        //END Content Patch patch1_ssvb_patch.tar.gz
        if (shouldPadParenthesies)
          {
            if (currentChar == '(' || currentChar == '[' )
              {
                char peekedChar = peekNextChar();

                isInPotentialCalculation = true;
                appendCurrentChar();
                if (!(currentChar == '(' && peekedChar == ')')
                    && !(currentChar == '[' && peekedChar == ']'))
                  appendSpacePad();
                continue;
              }
            else if (currentChar == ')' || currentChar == ']')
              {
                char peekedChar = peekNextChar();

                if (!(previousChar == '(' && currentChar == ')')
                    && !(previousChar == '[' && currentChar == ']'))
                  appendSpacePad();

                appendCurrentChar();

                if (peekedChar != ';' && peekedChar != ',' && peekedChar != '.'
                    && !(currentChar == ']' && peekedChar == '['))
                  appendSpacePad();
                continue;
              }
          }

        appendCurrentChar();
      }

    // return a beautified (i.e. correctly indented) line.

    string beautifiedLine;
    int readyFormattedLineLength = trim(readyFormattedLine).length();

    if (prependEmptyLine
        && readyFormattedLineLength > 0
        && previousReadyFormattedLineLength > 0)
      {
        isLineReady = true; // signal that a readyFormattedLine is still waiting
        beautifiedLine = beautify("");
      }
    else
      {
        isLineReady = false;
        beautifiedLine = beautify(readyFormattedLine);
      }

    prependEmptyLine = false;
    previousReadyFormattedLineLength = readyFormattedLineLength;

    return beautifiedLine;

  }


  /**
  * check if there are any indented lines ready to be read by nextLine()
  *
  * @return    are there any indented lines ready?
  */
  bool ASFormatter::hasMoreLines() const
    {
      if (!isFormattingEnabled())
        return ASBeautifier::hasMoreLines();
      else
        return !endOfCodeReached;
    }

  /**
   * check if formatting options are enabled, in addition to indentation.
   *
   * @return     are formatting options enabled?
   */
  bool ASFormatter::isFormattingEnabled() const
    {
      return (bracketFormatMode != NONE_MODE
              || shouldPadOperators
              || shouldConvertTabs);
    }

  /**
   * set the bracket formatting mode.
   * options:
   *    astyle::NONE_MODE     no formatting of brackets.
   *    astyle::ATTACH_MODE   Java, K&R style bracket placement.
   *    astyle::BREAK_MODE    ANSI C/C++ style bracket placement.
   *
   * @param mode         the bracket formatting mode.
   */
  void ASFormatter::setBracketFormatMode(BracketMode mode)
  {
    bracketFormatMode = mode;
  }

  /**
   * set closing header bracket breaking mode
   * options:
   *    true     brackets just before closing headers (e.g. 'else', 'catch')
   *             will be broken, even if standard brackets are attached.
   *    false    closing header brackets will be treated as standard brackets.
   *
   * @param mode         the closing header bracket breaking mode.
   */
  void ASFormatter::setBreakClosingHeaderBracketsMode(bool state)
  {
    shouldBreakClosingHeaderBrackets = state;
  }

  /**
   * set 'else if()' breaking mode
   * options:
   *    true     'else' headers will be broken from their succeeding 'if' headers.
   *    false    'else' headers will be attached to their succeeding 'if' headers.
   *
   * @param mode         the 'else if()' breaking mode.
   */
  void ASFormatter::setBreakElseIfsMode(bool state)
  {
    shouldBreakElseIfs = state;
  }

  /**
   * set operator padding mode.
   * options:
   *    true     statement operators will be padded with spaces around them.
   *    false    statement operators will not be padded.
   *
   * @param mode         the padding mode.
   */
  void ASFormatter::setOperatorPaddingMode(bool state)
  {
    shouldPadOperators = state;
  }

  /**
  * set parentheies padding mode.
  * options:
  *    true     statement parenthesies will be padded with spaces around them.
  *    false    statement parenthesies will not be padded.
  *
  * @param mode         the padding mode.
  */
  void ASFormatter::setParenthesisPaddingMode(bool state)
  {
    shouldPadParenthesies = state;
  }

  /**
   * set option to break/not break one-line blocks
   *
   * @param state        true = break, false = don't break.
   */
  void ASFormatter::setBreakOneLineBlocksMode(bool state)
  {
    shouldBreakOneLineBlocks = state;
  }

  /**
   * set option to break/not break lines consisting of multiple statements.
   *
   * @param state        true = break, false = don't break.
   */
  void ASFormatter::setSingleStatementsMode(bool state)
  {
    shouldBreakOneLineStatements = state;
  }

  /**
   * set option to convert tabs to spaces.
   *
   * @param state        true = convert, false = don't convert.
   */
  void ASFormatter::setTabSpaceConversionMode(bool state)
  {
    shouldConvertTabs = state;
  }


  /**
   * set option to break unrelated blocks of code with empty lines.
   *
   * @param state        true = convert, false = don't convert.
   */
  void ASFormatter::setBreakBlocksMode(bool state)
  {
    shouldBreakBlocks = state;
  }

  /**
   * set option to break closing header blocks of code (such as 'else', 'catch', ...) with empty lines.
   *
   * @param state        true = convert, false = don't convert.
   */
  void ASFormatter::setBreakClosingHeaderBlocksMode(bool state)
  {
    shouldBreakClosingHeaderBlocks = state;
  }

  /**
   * check if a specific sequence exists in the current placement of the current line
   *
   * @return             whether sequence has been reached.
   * @param sequence     the sequence to be checked
   */
  bool ASFormatter::isSequenceReached(const string &sequence) const
    {
      return currentLine.COMPARE(charNum, sequence.length(), sequence) == 0;

    }

  /**
   * jump over several characters.
   *
   * @param i       the number of characters to jump over.
   */
  void ASFormatter::goForward(int i)
  {
    while (--i >= 0)
      getNextChar();
  }

  /**
  * peek at the next unread character.
  *
  * @return     the next unread character.
  */
  char ASFormatter::peekNextChar() const
    {
      int peekNum = charNum + 1;
      int len = currentLine.length();
      char ch = ' ';

      while (peekNum < len)
        {
          ch = currentLine[peekNum++];
          if (!isWhiteSpace(ch))
            return ch;
        }

      if (shouldConvertTabs && ch == '\t')
        ch = ' ';

      return ch;
    }

  /**
  * check if current placement is before a comment or line-comment
  *
  * @return     is before a comment or line-comment.
  */
  bool ASFormatter::isBeforeComment() const
    {
      int peekNum = charNum + 1;
      int len = currentLine.length();
      // char ch = ' ';
      bool foundComment = false;

      for (peekNum = charNum + 1;
           peekNum < len && isWhiteSpace(currentLine[peekNum]);
           ++peekNum)
        ;

      if (peekNum < len)
        foundComment = ( currentLine.COMPARE(peekNum, 2, AS_OPEN_COMMENT) == 0
                         || currentLine.COMPARE(peekNum, 2, AS_OPEN_LINE_COMMENT) == 0 );

      return foundComment;
    }

  /**
  * get the next character, increasing the current placement in the process.
  * the new character is inserted into the variable currentChar.
  *
  * @return   whether succeded to recieve the new character.
  */
  bool ASFormatter::getNextChar()
  {
    isInLineBreak = false;
    bool isAfterFormattedWhiteSpace = false;

    if (shouldPadOperators && !isInComment && !isInLineComment
        && !isInQuote && !doesLineStartComment && !isInPreprocessor
        && !isBeforeComment())
      {
      //BEGIN Content Patch patch1_ssvb_patch.tar.gz
        char prevchar = ' ';
        char nextchar = peekNextChar();
        
        int len = formattedLine.length();
       // if (len > 0 && isWhiteSpace(formattedLine[len-1]))
       if (len > 0) prevchar = formattedLine[len-1];
        if (isWhiteSpace(prevchar) || prevchar == '(' || prevchar == '[' || 
                nextchar == ')' || nextchar == ']')
        {
          isAfterFormattedWhiteSpace = true;
        }
        //END Content Patch patch1_ssvb_patch.tar.gz
      }

    previousChar = currentChar;
    if (!isWhiteSpace(currentChar))
      {
        previousNonWSChar = currentChar;
        if (!isInComment && !isInLineComment && !isInQuote
            && !isSequenceReached(AS_OPEN_COMMENT)
            && !isSequenceReached(AS_OPEN_LINE_COMMENT) )
          previousCommandChar = previousNonWSChar;
      }

    unsigned int currentLineLength = currentLine.length();

    if (charNum+1 < currentLineLength
        && (!isWhiteSpace(peekNextChar()) || isInComment || isInLineComment))
      {
        currentChar = currentLine[++charNum];
        if (isAfterFormattedWhiteSpace)
          while (isWhiteSpace(currentChar) && charNum+1 < currentLineLength)
            currentChar = currentLine[++charNum];

        if (shouldConvertTabs && currentChar == '\t')
          currentChar = ' ';

        return true;
      }
    // BEGIN Content patch ASFormatter450670.patch.bz2
    else if (isInLineComment &&  (charNum+1 == currentLineLength))
    {
    	// fix BUG #450670
      currentChar = ' ';  
      return true;
    }
    // END Content patch ASFormatter450670.patch.bz2  
    else
      {
        if (sourceIterator->hasMoreLines())
          {
            currentLine = sourceIterator->nextLine();
            if (currentLine.length() == 0)
              {
                /*think*/ currentLine = string(" ");
              }

            // unless reading in the first line of the file,
            // break a new line.
            if (!isVirgin)
              isInLineBreak = true;
            else
              isVirgin = false;

            if (isInLineComment)
              isImmediatelyPostLineComment = true;
            isInLineComment = false;

            trimNewLine();
            currentChar = currentLine[charNum];

            // check if is in preprocessor right after the line break and line trimming
            if (previousNonWSChar != '\\')
              isInPreprocessor = false;

            if (shouldConvertTabs && currentChar == '\t')
              currentChar = ' ';

            return true;
          }
        else
          {
            endOfCodeReached = true;
            return false;
          }
      }
  }

  /**
  * jump over the leading white space in the current line,
  * IF the line does not begin a comment or is in a preprocessor definition.
  */
  void ASFormatter::trimNewLine()
  {
    unsigned int len = currentLine.length();
    charNum = 0;

    if (isInComment || isInPreprocessor)
      return;

    while (isWhiteSpace(currentLine[charNum]) && charNum+1 < len)
      ++charNum;

    doesLineStartComment = false;
    if (isSequenceReached(string("/*")))
      {
        charNum = 0;
        doesLineStartComment = true;
      }
  }

  /**
   * append a character to the current formatted line.
   * Unless disabled (via canBreakLine == false), first check if a 
   * line-break has been registered, and if so break the 
   * formatted line, and only then append the character into
   * the next formatted line.
   *
   * @param ch               the character to append.
   * @param canBreakLine     if true, a registered line-break
   */
  void ASFormatter::appendChar(char ch, bool canBreakLine)
  {
    if (canBreakLine && isInLineBreak)
      breakLine();
    formattedLine.append(1, ch);
  }

  /**
   * append the CURRENT character (curentChar)to the current
   * formatted line. Unless disabled (via canBreakLine == false),
   * first check if a line-break has been registered, and if so
   * break the formatted line, and only then append the character
   * into the next formatted line.
   *
   * @param canBreakLine     if true, a registered line-break
   */
  void ASFormatter::appendCurrentChar(bool canBreakLine)
  {
    appendChar(currentChar, canBreakLine);
  }

  /**
   * append a string sequence to the current formatted line.
   * Unless disabled (via canBreakLine == false), first check if a 
   * line-break has been registered, and if so break the 
   * formatted line, and only then append the sequence into
   * the next formatted line.
   *
   * @param sequence         the sequence to append.
   * @param canBreakLine     if true, a registered line-break
   */
  void ASFormatter::appendSequence(const string &sequence, bool canBreakLine)
  {
    if (canBreakLine && isInLineBreak)
      breakLine();
    formattedLine.append(sequence);
  }

  /**
   * append a space to the current formattedline, UNLESS the 
   * last character is already a white-space character.
   */
  void ASFormatter::appendSpacePad()
  {
    int len = formattedLine.length();
    if (len == 0 || !isWhiteSpace(formattedLine[len-1]))
      formattedLine.append(1, ' ');
  }

  /**
   * register a line break for the formatted line.
   */
  void ASFormatter::breakLine()
  {
    isLineReady = true;
    isInLineBreak = false;

    // queue an empty line prepend request if one exists
    prependEmptyLine = isPrependPostBlockEmptyLineRequested;

    readyFormattedLine =  formattedLine;
    if (isAppendPostBlockEmptyLineRequested)
      {
        isAppendPostBlockEmptyLineRequested = false;
        isPrependPostBlockEmptyLineRequested = true;
      }
    else
      {
        isPrependPostBlockEmptyLineRequested = false;
      }

    formattedLine = "";
  }

  /**
   * check if the currently reached open-bracket (i.e. '{')
   * opens a:
   * - a definition type block (such as a class or namespace),
   * - a command block (such as a method block)
   * - a static array
   * this method takes for granted that the current character
   * is an opening bracket.
   *
   * @return    the type of the opened block.
   */
  BracketType ASFormatter::getBracketType() const
    {
      BracketType returnVal;

      if (foundPreDefinitionHeader)
        returnVal = DEFINITION_TYPE;
      else
        {
          bool isCommandType;
          isCommandType = ( foundPreCommandHeader
                            || ( currentHeader != NULL && isNonParenHeader )
                            || ( previousCommandChar == ')' )
                            || ( previousCommandChar == ':' && !foundQuestionMark )
                            || ( previousCommandChar == ';' )
                            || ( ( previousCommandChar == '{' ||  previousCommandChar == '}')
                                 && isPreviousBracketBlockRelated ) );

          returnVal = (isCommandType ? COMMAND_TYPE : ARRAY_TYPE);
        }

      if (isOneLineBlockReached())
        returnVal = (BracketType) (returnVal | SINGLE_LINE_TYPE);

      return returnVal;
    }

  /**
   * check if the currently reached  '*' or '&' character is
   * a pointer-or-reference symbol, or another operator.
   * this method takes for granted that the current character
   * is either a '*' or '&'.
   *
   * @return        whether current character is a reference-or-pointer 
   */
  bool ASFormatter::isPointerOrReference() const
    {
      bool isPR;
      isPR = ( !isInPotentialCalculation
               || IS_A(bracketTypeStack->back(), DEFINITION_TYPE)
               || (!isLegalNameChar(previousNonWSChar)
                   && previousNonWSChar != ')'
                   && previousNonWSChar != ']')
             );

      if (!isPR)
        {
          char nextChar = peekNextChar();
          isPR |= (!isWhiteSpace(nextChar)
                   && nextChar != '-'
                   && nextChar != '('
                   && nextChar != '['
                   && !isLegalNameChar(nextChar));
        }

      return isPR;
    }


  /**
   * check if the currently reached '-' character is
   * a urinary minus
   * this method takes for granted that the current character
   * is a '-'.
   *
   * @return        whether the current '-' is a urinary minus.
   */
  bool ASFormatter::isUrinaryMinus() const
    {
      return ( (previousOperator == &AS_RETURN || !isalnum(previousCommandChar))
               && previousCommandChar != '.'
               && previousCommandChar != ')'
               && previousCommandChar != ']' );
    }


  /**
   * check if the currently reached '-' or '+' character is
   * part of an exponent, i.e. 0.2E-5.
   * this method takes for granted that the current character
   * is a '-' or '+'.
   *
   * @return        whether the current '-' is in an exponent.
   */
  bool ASFormatter::isInExponent() const
    {
      int formattedLineLength = formattedLine.length();
      if (formattedLineLength >= 2)
        {
          char prevPrevFormattedChar = formattedLine[formattedLineLength - 2];
          char prevFormattedChar = formattedLine[formattedLineLength - 1];

          return ( (prevFormattedChar == 'e' || prevFormattedChar == 'E')
                   && (prevPrevFormattedChar == '.' || isdigit(prevPrevFormattedChar)) );
        }
      else
        return false;
    }

  /**
   * check if a one-line bracket has been reached,
   * i.e. if the currently reached '{' character is closed
   * with a complimentry '}' elsewhere on the current line,
   *.
   * @return        has a one-line bracket been reached?
   */
  bool ASFormatter::isOneLineBlockReached() const
    {
      bool isInComment = false;
      bool isInQuote = false;
      int bracketCount = 1;
      int currentLineLength = currentLine.length();
      int i = 0;
      char ch = ' ';
      char quoteChar = ' ';

      for (i = charNum + 1; i < currentLineLength; ++i)
        {
          ch = currentLine[i];

          if (isInComment)
            {
              if (currentLine.COMPARE(i, 2, "*/") == 0)
                {
                  isInComment = false;
                  ++i;
                }
              continue;
            }

          if (ch == '\\')
            {
              ++i;
              continue;
            }

          if (isInQuote)
            {
              if (ch == quoteChar)
                isInQuote = false;
              continue;
            }

          if (ch == '"' || ch == '\'')
            {
              isInQuote = true;
              quoteChar = ch;
              continue;
            }

          if (currentLine.COMPARE(i, 2, "//") == 0)
            break;

          if (currentLine.COMPARE(i, 2, "/*") == 0)
            {
              isInComment = true;
              ++i;
              continue;
            }

          if (ch == '{')
            ++bracketCount;
          else if (ch == '}')
            --bracketCount;

          if(bracketCount == 0)
            return true;
        }

      return false;
    }


  /**
   * check if one of a set of headers has been reached in the
   * current position of the current line.
   *
   * @return             a pointer to the found header. Or a NULL if no header has been reached.
   * @param headers      a vector of headers
   * @param checkBoundry 
   */
  const string *ASFormatter::findHeader(const vector<const string*> &headers, bool checkBoundry)
  {
    return ASBeautifier::findHeader(currentLine, charNum, headers, checkBoundry);
  }



#ifdef USES_NAMESPACE
}
#endif
/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * compiler_defines.h   (1 January 1999)
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C++, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 */


#ifndef ASFORMATTER_H
#define ASFORMATTER_H

#include "ASBeautifier.h"
//#include "enums.h"
#include "compiler_defines.h"

namespace astyle {

  class ASFormatter : public ASBeautifier
    {
    public:
      ASFormatter();
      virtual ~ASFormatter();
      virtual void init(ASSourceIterator* iter);
      virtual bool hasMoreLines() const;
      virtual string nextLine();
      void setBracketFormatMode(BracketMode mode);
      void setBreakClosingHeaderBracketsMode(bool state);
      void setOperatorPaddingMode(bool mode);
      void setParenthesisPaddingMode(bool mode);
      void setBreakOneLineBlocksMode(bool state);
      void setSingleStatementsMode(bool state);
      void setTabSpaceConversionMode(bool state);
      void setBreakBlocksMode(bool state);
      void setBreakClosingHeaderBlocksMode(bool state);
      void setBreakElseIfsMode(bool state);

    private:
      void ASformatter(ASFormatter &copy); // not to be imlpemented
      void operator=(ASFormatter&); // not to be implemented
      void staticInit();
      bool isFormattingEnabled() const;
      void goForward(int i);
      bool getNextChar();
      char peekNextChar() const;
      bool isBeforeComment() const;
      void trimNewLine();
      BracketType getBracketType() const;
      bool isPointerOrReference() const;
      bool isUrinaryMinus() const;
      bool isInExponent() const;
      bool isOneLineBlockReached() const;
      void appendChar(char ch, bool canBreakLine = true);
      void appendCurrentChar(bool canBreakLine = true);
      void appendSequence(const string &sequence, bool canBreakLine = true);
      void appendSpacePad();
      void breakLine();
      inline bool isSequenceReached(const string &sequence) const;
      const string *findHeader(const vector<const string*> &headers, bool checkBoundry = true);

      static vector<const string*> headers;
      static vector<const string*> nonParenHeaders;
      static vector<const string*> preprocessorHeaders;
      static vector<const string*> preDefinitionHeaders;
      static vector<const string*> preCommandHeaders;
      static vector<const string*> operators;
      static vector<const string*> assignmentOperators;
      static bool calledInitStatic;

      ASSourceIterator *sourceIterator;
      vector<const string*> *preBracketHeaderStack;
      vector<BracketType> *bracketTypeStack;
      vector<int> *parenStack;
      string readyFormattedLine;
      string currentLine;
      string formattedLine;
      const string *currentHeader;
      const string *previousOperator;
      char currentChar;
      char previousChar;
      char previousNonWSChar;
      char previousCommandChar;
      char quoteChar;
      unsigned int charNum;
      BracketMode bracketFormatMode;
      bool isVirgin;
      bool shouldPadOperators;
      bool shouldPadParenthesies;
      bool shouldConvertTabs;
      bool isInLineComment;
      bool isInComment;
      bool isInPreprocessor;
      bool isInTemplate;			// true both in template definitions (e.g. template<class A>) and template usage (e.g. F<int>).
      bool doesLineStartComment;
      bool isInQuote;
      bool isSpecialChar;
      bool isNonParenHeader;
      bool foundQuestionMark;
      bool foundPreDefinitionHeader;
      bool foundPreCommandHeader;
      bool isInLineBreak;
      bool isInClosingBracketLineBreak;
      bool endOfCodeReached;
      bool isLineReady;
      bool isPreviousBracketBlockRelated;
      bool isInPotentialCalculation;
      //bool foundOneLineBlock;
      bool shouldBreakOneLineBlocks;
      bool shouldReparseCurrentChar;
      bool shouldBreakOneLineStatements;
      bool shouldBreakLineAfterComments;
      bool shouldBreakClosingHeaderBrackets;
      bool shouldBreakElseIfs;
      bool passedSemicolon;
      bool passedColon;
      bool isImmediatelyPostComment;
      bool isImmediatelyPostLineComment;
      bool isImmediatelyPostEmptyBlock;

      bool shouldBreakBlocks;
      bool shouldBreakClosingHeaderBlocks;
      bool isPrependPostBlockEmptyLineRequested;
      bool isAppendPostBlockEmptyLineRequested;

      bool prependEmptyLine;
      bool foundClosingHeader;
      int previousReadyFormattedLineLength;

      bool isInHeader;
      bool isImmediatelyPostHeader;

    };

}

#endif
/*
* Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
*
* ASResource.cpp
* by Tal Davidson (davidsont@bigfoot.com)
* This file is a part of "Artistic Style" - an indentater and reformatter
* of C, C, C# and Java source files.
*
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
*/

#include "compiler_defines.h"
#include "ASResource.h"

#include <string>


#ifdef USES_NAMESPACE
using namespace std;

namespace astyle
  {
#endif

  const string ASResource::AS_IF = string("if");
  const string ASResource::AS_ELSE = string ("else");
  const string ASResource::AS_FOR = string("for");
  const string ASResource::AS_DO = string("do");
  const string ASResource::AS_WHILE = string("while");
  const string ASResource::AS_SWITCH = string ("switch");
  const string ASResource::AS_CASE = string ("case");
  const string ASResource::AS_DEFAULT = string("default");
  const string ASResource::AS_CLASS = string("class");
  const string ASResource::AS_STRUCT = string("struct");
  const string ASResource::AS_UNION = string("union");
  const string ASResource::AS_INTERFACE = string("interface");
  const string ASResource::AS_NAMESPACE = string("namespace");
  const string ASResource::AS_EXTERN = string("extern");
  const string ASResource::AS_PUBLIC = string("public");
  const string ASResource::AS_PROTECTED = string("protected");
  const string ASResource::AS_PRIVATE = string("private");
  const string ASResource::AS_STATIC = string("static");
  const string ASResource::AS_SYNCHRONIZED = string("synchronized");
  const string ASResource::AS_OPERATOR = string("operator");
  const string ASResource::AS_TEMPLATE = string("template");
  const string ASResource::AS_TRY = string("try");
  const string ASResource::AS_CATCH = string("catch");
  const string ASResource::AS_FINALLY = string("finally");
  const string ASResource::AS_THROWS = string("throws");
  const string ASResource::AS_CONST = string("const");

  const string ASResource::AS_ASM = string("asm");

  const string ASResource::AS_BAR_DEFINE = string("#define");
  const string ASResource::AS_BAR_INCLUDE = string("#include");
  const string ASResource::AS_BAR_IF = string("#if");
  const string ASResource::AS_BAR_EL = string("#el");
  const string ASResource::AS_BAR_ENDIF = string("#endif");

  const string ASResource::AS_OPEN_BRACKET = string("{");
  const string ASResource::AS_CLOSE_BRACKET = string("}");
  const string ASResource::AS_OPEN_LINE_COMMENT = string("//");
  const string ASResource::AS_OPEN_COMMENT = string("/*");
  const  string ASResource::AS_CLOSE_COMMENT = string("*/");

  const string ASResource::AS_ASSIGN = string("=");
  const string ASResource::AS_PLUS_ASSIGN = string("+=");
  const string ASResource::AS_MINUS_ASSIGN = string("-=");
  const string ASResource::AS_MULT_ASSIGN = string("*=");
  const string ASResource::AS_DIV_ASSIGN = string("/=");
  const string ASResource::AS_MOD_ASSIGN = string("%=");
  const string ASResource::AS_OR_ASSIGN = string("|=");
  const string ASResource::AS_AND_ASSIGN = string("&=");
  const string ASResource::AS_XOR_ASSIGN = string("^=");
  const string ASResource::AS_GR_GR_ASSIGN = string(">>=");
  const string ASResource::AS_LS_LS_ASSIGN = string("<<=");
  const string ASResource::AS_GR_GR_GR_ASSIGN = string(">>>=");
  const string ASResource::AS_LS_LS_LS_ASSIGN = string("<<<=");
  const string ASResource::AS_RETURN = string("return");

  const string ASResource::AS_EQUAL = string("==");
  const string ASResource::AS_PLUS_PLUS = string("++");
  const string ASResource::AS_MINUS_MINUS = string("--");
  const string ASResource::AS_NOT_EQUAL = string("!=");
  const string ASResource::AS_GR_EQUAL = string(">=");
  const string ASResource::AS_GR_GR = string(">>");
  const string ASResource::AS_GR_GR_GR = string(">>>");
  const string ASResource::AS_LS_EQUAL = string("<=");
  const string ASResource::AS_LS_LS = string("<<");
  const string ASResource::AS_LS_LS_LS = string("<<<");
  const string ASResource::AS_ARROW = string("->");
  const string ASResource::AS_AND = string("&&");
  const string ASResource::AS_OR = string("||");
  const string ASResource::AS_COLON_COLON = string("::");
  const string ASResource::AS_PAREN_PAREN = string("()");
  const string ASResource::AS_BLPAREN_BLPAREN = string("[]");

  const string ASResource::AS_PLUS = string("+");
  const string ASResource::AS_MINUS = string("-");
  const string ASResource::AS_MULT = string("*");
  const string ASResource::AS_DIV = string("/");
  const string ASResource::AS_MOD = string("%");
  const string ASResource::AS_GR = string(">");
  const string ASResource::AS_LS = string("<");
  const string ASResource::AS_NOT = string("!");
  const string ASResource::AS_BIT_OR = string("|");
  const string ASResource::AS_BIT_AND = string("&");
  const string ASResource::AS_BIT_NOT = string("~");
  const string ASResource::AS_BIT_XOR = string("^");
  const string ASResource::AS_QUESTION = string("?");
  const string ASResource::AS_COLON = string(":");
  const string ASResource::AS_COMMA = string(",");
  const string ASResource::AS_SEMICOLON = string(";");

  const string ASResource::AS_FOREACH = string("foreach");
  const string ASResource::AS_LOCK = string("lock");
  const string ASResource::AS_UNSAFE = string("unsafe");
  const string ASResource::AS_FIXED = string("fixed");
  const string ASResource::AS_GET = string("get");
  const string ASResource::AS_SET = string("set");
  const string ASResource::AS_ADD = string("add");
  const string ASResource::AS_REMOVE = string("remove");

#ifdef USES_NAMESPACE
}
#endif


/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * compiler_defines.h   (1 January 1999)
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C++, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 */


#ifndef ASRES_H
#define ASRES_H

#include "compiler_defines.h"
#include "ASStreamIterator.h"

#include <iostream>
#include <fstream>
#include <string>

namespace astyle {

class ASResource
  {
  public:
    static const string AS_IF, AS_ELSE;
    static const string AS_DO, AS_WHILE;
    static const string AS_FOR;
    static const string AS_SWITCH, AS_CASE, AS_DEFAULT;
    static const string AS_TRY, AS_CATCH, AS_THROWS, AS_FINALLY;
    static const string AS_PUBLIC, AS_PROTECTED, AS_PRIVATE;
    static const string AS_CLASS, AS_STRUCT, AS_UNION, AS_INTERFACE, AS_NAMESPACE, AS_EXTERN;
    static const string AS_STATIC;
    static const string AS_CONST;
    static const string AS_SYNCHRONIZED;
    static const string AS_OPERATOR, AS_TEMPLATE;
    static const string AS_OPEN_BRACKET, AS_CLOSE_BRACKET;
    static const string AS_OPEN_LINE_COMMENT, AS_OPEN_COMMENT, AS_CLOSE_COMMENT;
    static const string AS_BAR_DEFINE, AS_BAR_INCLUDE, AS_BAR_IF, AS_BAR_EL, AS_BAR_ENDIF;
    static const string AS_RETURN;
    static const string AS_ASSIGN, AS_PLUS_ASSIGN, AS_MINUS_ASSIGN, AS_MULT_ASSIGN;
    static const string AS_DIV_ASSIGN, AS_MOD_ASSIGN, AS_XOR_ASSIGN, AS_OR_ASSIGN, AS_AND_ASSIGN;
    static const string AS_GR_GR_ASSIGN, AS_LS_LS_ASSIGN, AS_GR_GR_GR_ASSIGN, AS_LS_LS_LS_ASSIGN;
    static const string AS_EQUAL, AS_PLUS_PLUS, AS_MINUS_MINUS, AS_NOT_EQUAL, AS_GR_EQUAL, AS_GR_GR_GR, AS_GR_GR;
    static const string AS_LS_EQUAL, AS_LS_LS_LS, AS_LS_LS, AS_ARROW, AS_AND, AS_OR;
    static const string AS_COLON_COLON, AS_PAREN_PAREN, AS_BLPAREN_BLPAREN;
    static const string AS_PLUS, AS_MINUS, AS_MULT, AS_DIV, AS_MOD, AS_GR, AS_LS;
    static const string AS_NOT, AS_BIT_XOR, AS_BIT_OR, AS_BIT_AND, AS_BIT_NOT;
    static const string AS_QUESTION, AS_COLON, AS_SEMICOLON, AS_COMMA;
    static const string AS_ASM;
    static const string AS_FOREACH, AS_LOCK, AS_UNSAFE, AS_FIXED;
    static const string AS_GET, AS_SET, AS_ADD, AS_REMOVE;
  };
}
#endif

#ifndef ASSOURCEITERATOR_H
#define ASSOURCEITERATOR_H

#include <string>
#include "compiler_defines.h"

namespace astyle
  {

  class ASSourceIterator
    {
    public:
      virtual bool hasMoreLines() const = 0;
      virtual std::string nextLine() = 0;
    };
}

#endif
#include "compiler_defines.h"
#include "ASStreamIterator.h"

#include <iostream>
#include <fstream>
#include <string>

using namespace astyle;

ASStreamIterator::ASStreamIterator(istream *in)
{
  inStream = in;
}

ASStreamIterator::~ASStreamIterator()
{
  delete inStream;
}


bool ASStreamIterator::hasMoreLines() const
  {
    if (*inStream)
      return true;
    else
      return false;
  }

/*
string ASStreamIterator::nextLine()
{
   char theInChar;
   char peekedChar;
   int  theBufferPosn = 0;
 
   //
   // treat '\n', '\r', '\n\r' and '\r\n' as an endline.
   //
   while (theBufferPosn < 2047 && inStream->get(theInChar))
   // while not eof
   {
      if (theInChar != '\n' && theInChar != '\r')
      {
	 buffer[theBufferPosn] = theInChar;
         theBufferPosn++;
      }
      else
      {
	peekedChar = inStream->peek();
	if (peekedChar != theInChar && (peekedChar == '\r' || peekedChar == '\n') )
         {
            inStream->get(theInChar);
         }
         break;
      }
   }
   buffer[theBufferPosn] = '\0';
 
   return string(buffer);
}
*/


string ASStreamIterator::nextLine()
{
  char *srcPtr;
  char *filterPtr;

  inStream->getline(buffer, 2047);
  srcPtr = filterPtr = buffer;

  while (*srcPtr != 0)
    {
      if (*srcPtr != '\r')
        *filterPtr++ = *srcPtr;
      srcPtr++;
    }
  *filterPtr = 0;

  return string(buffer);
}

/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * compiler_defines.h   (1 January 1999)
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C++, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 */


#ifndef ASSTREAMITERATOR_H
#define ASSTREAMITERATOR_H

#include "ASSourceIterator.h"

using namespace std;

namespace astyle
  {
  class ASStreamIterator :
        public ASSourceIterator
    {
    public:
      ASStreamIterator(istream *in);
      virtual ~ASStreamIterator();
      bool hasMoreLines() const;
      string nextLine();

    private:
      istream * inStream;
      char buffer[2048];
    };

}

#endif
/***************************************************************************
                          charcodes.cpp  -  description
                             -------------------
    begin                : Wed Nov 24 2003
    copyright            : (C) 2003 by André imon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

// FILE SHOULD BE REMOVED FROM PROJECT

#ifndef CHAR_CODES
#define CHAR_CODES

#ifdef _WIN32

#define AUML_LC 228
#define OUML_LC 246
#define UUML_LC 252

#define AUML_UC 196
#define OUML_UC 214
#define UUML_UC 220


#define AACUTE_LC 225
#define EACUTE_LC 233
#define OACUTE_LC 243
#define UACUTE_LC 250

#define AACUTE_UC 193
#define EACUTE_UC 201
#define OACUTE_UC 211
#define UACUTE_UC 218

#define AGRAVE_LC 224
#define EGRAVE_LC 232
#define OGRAVE_LC 242
#define UGRAVE_LC 249

#define AGRAVE_UC 192
#define EGRAVE_UC 200
#define OGRAVE_UC 210
#define UGRAVE_UC 217

#define SZLIG 223

/* DOS CONSOLE CODES
#define AUML_LC 132
#define OUML_LC 148
#define UUML_LC 129

#define AUML_UC 142
#define OUML_UC 153
#define UUML_UC 154


#define AACUTE_LC 160
#define EACUTE_LC 130
#define OACUTE_LC 162
#define UACUTE_LC 163

#define AACUTE_UC 181
#define EACUTE_UC 144
#define OACUTE_UC 224
#define UACUTE_UC 233

#define AGRAVE_LC 133
#define EGRAVE_LC 138
#define OGRAVE_LC 149
#define UGRAVE_LC 151

#define AGRAVE_UC 183
#define EGRAVE_UC 212
#define OGRAVE_UC 227
#define UGRAVE_UC 235

#define SZLIG 225
*/

#else

#define AUML_LC 164
#define OUML_LC 182
#define UUML_LC 188

#define AUML_UC 132
#define OUML_UC 150
#define UUML_UC 156


#define AACUTE_LC 161
#define EACUTE_LC 169
#define OACUTE_LC 179
#define UACUTE_LC 186

#define AACUTE_UC 129
#define EACUTE_UC 137
#define OACUTE_UC 147
#define UACUTE_UC 154

#define AGRAVE_LC 160
#define EGRAVE_LC 168
#define OGRAVE_LC 178
#define UGRAVE_LC 185

#define AGRAVE_UC 128
#define EGRAVE_UC 136
#define OGRAVE_UC 146
#define UGRAVE_UC 153

#define SZLIG 159

#endif

#endif
/***************************************************************************
                          cmdlineoptions.cpp  -  description
                             -------------------
    begin                : Sun Nov 25 2001
    copyright            : (C) 2001 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "cmdlineoptions.h"

using namespace std;

/* Siehe man getopt (3)
   Konstruktor legt Optionen und Argumente fest
*/
CmdLineOptions::CmdLineOptions(int argc, char *argv[]):
    numberSpaces(0),
    wrappingStyle(highlight::WRAP_DISABLED),
    outputType (highlight::HTML),
    opt_language (false),
    opt_include_style (false),
    opt_help (false),
    opt_version (false),
    opt_verbose (false),
    opt_linenumbers (false),
    opt_style (false),
    opt_batch_mode (false),
    opt_fragment (false) ,
    opt_attach_line_anchors (false),
    opt_show_themes (false),
    opt_show_langdefs (false),
    opt_printindex(false),
    opt_quiet(false),
    opt_xslfo_fop(false),
    opt_replacequotes(false),
    opt_print_progress(false),
    opt_fill_zeroes(false),
    opt_stylepath_explicit(false),
    opt_force_output(false),
    configFileRead(false),
    helpLang("en"),
    charset("ISO-8859-1")
{

  loadConfigurationFile();

  int c, option_index = 0;
  static struct option long_options[] =
      {
        {OPT_OUT, 1, 0, S_OPT_OUT},
        {OPT_IN, 1, 0, S_OPT_IN},
        {OPT_SYNTAX, 1, 0, S_OPT_SYNTAX},
        {OPT_VERBOSE, 0, 0, S_OPT_VERBOSE},
        {OPT_INC_STYLE, 0, 0, S_OPT_INC_STYLE},
        {OPT_HELP, 0, 0, S_OPT_HELP},
        {OPT_HELPINT, 1, 0, S_OPT_HELPINT},
        {OPT_LINENO,0,0,S_OPT_LINENO},
        {OPT_STYLE, 1,0,S_OPT_STYLE},
        {OPT_STYLE_OUT, 1, 0,S_OPT_STYLE_OUT},
        {OPT_STYLE_IN, 1, 0,S_OPT_STYLE_IN},
        {OPT_DELTABS,1,0,S_OPT_DELTABS},
        {OPT_XHTML, 0,0,S_OPT_XHTML},
        {OPT_RTF, 0,0,S_OPT_RTF},
        {OPT_TEX,0, 0,S_OPT_TEX},
        {OPT_LATEX,0, 0,S_OPT_LATEX},
        {OPT_XSLFO,0, 0,S_OPT_XSLFO},
        {OPT_ANSI,0, 0,S_OPT_ANSI},
        {OPT_XML,0, 0,S_OPT_XML},
        {OPT_BATCHREC,1,0,S_OPT_BATCHREC},
        {OPT_FRAGMENT,0,0,S_OPT_FRAGMENT},
        {OPT_ANCHORS, 0,0,S_OPT_ANCHORS },
        {OPT_LISTTHEMES, 0,0,S_OPT_LISTTHEMES },
        {OPT_LISTLANGS, 0,0,S_OPT_LISTLANGS },
        {OPT_OUTDIR,1,0,S_OPT_OUTDIR},
        {OPT_VERSION,0,0,0},
        {OPT_FORMATSTYLE,1,0,S_OPT_FORMATSTYLE},
        {OPT_DATADIR,1,0,S_OPT_DATADIR},
        {OPT_ADDDATADIR,1,0,S_OPT_ADDDATADIR},
        {OPT_INDEXFILE,0,0,S_OPT_INDEXFILE},
        {OPT_WRAP,0,0,S_OPT_WRAP},
        {OPT_WRAPSIMPLE,0,0,S_OPT_WRAPSIMPLE},
        {OPT_QUIET,0,0,S_OPT_QUIET},
        {OPT_REPLACE_QUOTES,0,0,S_OPT_REPLACE_QUOTES},
        {OPT_PROGRESSBAR,0,0,S_OPT_PROGRESSBAR},
        {OPT_FILLZEROES,0,0,S_OPT_FILLZEROES},
        {OPT_ENCODING,1,0,S_OPT_ENCODING},

        //remove as soon as APAche fixes the bug in FOP (0.20.5)
        {OPT_FOP,0,0,S_OPT_FOP},

        //deprecated
        {OPT_CSSOUT,1,0,0},
        {OPT_CSSIN,1,0,0},
        {OPT_INC_CSS,0,0,0},
        {OPT_FORCE_OUTPUT,0,0,0},

        {0, 0, 0, 0}
      };

  while (1)
    {
      c = getopt_long (argc, argv,S_OPTIONS_STRING,long_options, &option_index);
      if (c == -1)
        break;

      switch (c)
        {
        case 0:   // long options
          if (long_options[option_index].name==OPT_VERSION) {
              opt_version = true;
            }
            if (long_options[option_index].name==OPT_CSSOUT) {
                styleOutFilename=string(optarg);
                printDeprecatedWarning(OPT_CSSOUT, OPT_STYLE_OUT);
            }
            if (long_options[option_index].name==OPT_CSSIN) {
                styleInFilename=string(optarg);
                printDeprecatedWarning(OPT_CSSIN, OPT_STYLE_IN);
            }
            if (long_options[option_index].name==OPT_INC_CSS) {
                opt_include_style = true;
                printDeprecatedWarning(OPT_INC_CSS, OPT_INC_STYLE);
            }
            if (long_options[option_index].name==OPT_FORCE_OUTPUT) {
                opt_force_output = true;
            }
          break;
        case S_OPT_OUT:
          outFilename=string(optarg);
          break;
        case S_OPT_IN:
          inputFileNames.push_back(string(optarg));
          break;
        case S_OPT_STYLE_OUT:
          styleOutFilename=string(optarg);
          opt_stylepath_explicit=true;
          break;
        case S_OPT_STYLE_IN:
          styleInFilename=string(optarg);
          break;
        case S_OPT_VERBOSE:
          opt_verbose = true;
          break;
        case S_OPT_QUIET:
          opt_quiet = true;
          break;
         case S_OPT_INC_STYLE:
          opt_include_style = true;
          break;
        case S_OPT_HELPINT:
          helpLang=string(optarg);
        case S_OPT_HELP:
          opt_help = true;
          break;
        case S_OPT_LINENO:
          opt_linenumbers = true;
          break;
        case '?':
          //opt_help = true;
          break;
        case S_OPT_STYLE:
          styleName=string(optarg);
          opt_style = true;
          break;
        case S_OPT_SYNTAX:
          language=string(optarg);
          opt_language = true;
          break;
        case S_OPT_DELTABS:
          numberSpaces = StringTools::str2int (string(optarg));
          break;
        case S_OPT_XHTML:
          outputType=highlight::XHTML;
          break;
        case S_OPT_RTF:
          outputType=highlight::RTF;
          break;
        case S_OPT_TEX:
          outputType=highlight::TEX;
          break;
        case S_OPT_LATEX:
          outputType=highlight::LATEX;
          break;
        case S_OPT_XSLFO:
          outputType=highlight::XSLFO;
          break;
        case S_OPT_ANSI:
          outputType=highlight::ANSI;
          break;
        case S_OPT_XML:
          outputType=highlight::XML;
          break;
        case S_OPT_BATCHREC:
          opt_batch_mode = true;
          readDirectory(string(optarg));
          break;
        case S_OPT_FRAGMENT:
          opt_fragment = true;
          break;
        case S_OPT_ANCHORS:
          opt_attach_line_anchors = true;
          break;
        case S_OPT_LISTTHEMES:
          opt_show_themes = true;
          break;
        case S_OPT_LISTLANGS:
          opt_show_langdefs = true;
          break;
        case S_OPT_OUTDIR:
          outDirectory = validateDirPath(string(optarg));
          break;
        case S_OPT_FORMATSTYLE:
          indentScheme =string(optarg);
          break;
        case S_OPT_ENCODING:
          charset =string(optarg);
          break;
        case S_OPT_DATADIR:
          dataDir=validateDirPath(string(optarg));
          break;
        case S_OPT_ADDDATADIR:
          additionalDataDir=validateDirPath(string(optarg));
          break;
         case S_OPT_INDEXFILE:
          opt_printindex=true;
          break;
         case S_OPT_WRAPSIMPLE:
          wrappingStyle = highlight::WRAP_SIMPLE;
          break;
         case S_OPT_WRAP:
          wrappingStyle = highlight::WRAP_DEFAULT;
          break;
         case S_OPT_FOP:
          opt_xslfo_fop=true;
          break;
         case S_OPT_REPLACE_QUOTES:
          opt_replacequotes=true;
          break;
         case S_OPT_PROGRESSBAR:
          opt_print_progress=true;
          break;
         case S_OPT_FILLZEROES:
          opt_fill_zeroes=true;
          break;
        default:
          cerr <<"higlight: Unknown option " <<c<< endl;
        }
    }

  if (optind < argc)   //still args left
    {
      if  (inputFileNames.empty()) {
        while (optind < argc){
          inputFileNames.push_back(string(argv[optind++]));
         }
      }
    } else if (inputFileNames.empty()) {
       inputFileNames.push_back("");
    }
  if (printDebugInfo() && configFileRead) {
    cout << "Configuration file \""<<configFilePath<<"\" was read.\n";
  }
}

CmdLineOptions::~CmdLineOptions(){
}

const string &CmdLineOptions::getSingleOutFilename()
  {
   if (!inputFileNames.empty() && !outDirectory.empty()) {
      if (outFilename.empty()) {
        outFilename = outDirectory;
        int delim = getSingleInFilename().find_last_of(Platform::pathSeparator)+1;
        outFilename += getSingleInFilename().substr((delim>-1)?delim:0)
                       + getOutFileSuffix();
      }
   }
   return outFilename;
  }

const string &CmdLineOptions::getSingleInFilename()  const
  {
  return inputFileNames[0];
  }

const string &CmdLineOptions::getOutDirectory()
  {
    if (!outFilename.empty() && !enableBatchMode()){
      outDirectory=getDirName(outFilename);
    }
    return outDirectory;
  }

const string CmdLineOptions::getStyleOutFilename() const
  {
      if (!styleOutFilename.empty()) return styleOutFilename;
      return (outputType==highlight::HTML ||
              outputType==highlight::XHTML)? "highlight.css":"highlight.sty";
  }
const string &CmdLineOptions::getStyleInFilename() const
  {
    return styleInFilename;
  }
int CmdLineOptions::getNumberSpaces() const
  {
    return numberSpaces;
  }
bool CmdLineOptions::printVersion()const
  {
    return opt_version;
  }
bool CmdLineOptions::printHelp()const
  {
    return opt_help;
  }
bool CmdLineOptions::printDebugInfo()const
  {
    return opt_verbose;
  }
bool CmdLineOptions::quietMode()const
  {
    return opt_quiet;
  }
bool CmdLineOptions::includeStyleDef()const
  {
      return opt_include_style;
  }

bool CmdLineOptions::formatSupportsExtStyle(){
      return outputType==highlight::HTML ||
             outputType==highlight::XHTML ||
             outputType==highlight::LATEX ||
             outputType==highlight::TEX;
}

bool CmdLineOptions::printLineNumbers()const
  {
    return opt_linenumbers;
  }

string CmdLineOptions::getStyleName()const
  {
    return ( ( opt_style) ? styleName+".style" : "kwrite.style" );
  }
bool CmdLineOptions::enableBatchMode()const{
    return inputFileNames.size()>1 || opt_batch_mode;
}
bool CmdLineOptions::fragmentOutput()const{
    return opt_fragment;
}
string CmdLineOptions::getOutFileSuffix()const{
    switch (outputType){
      case highlight::XHTML: return ".xhtml";
      case highlight::RTF:   return ".rtf";
      case highlight::TEX:
      case highlight::LATEX: return ".tex";
      case highlight::XSLFO: return ".fo";
      case highlight::XML:   return ".xml";
      default:    return ".html";
    }
}
string CmdLineOptions::getDirName(const string & path) {
  size_t dirNameLength=path.rfind(Platform::pathSeparator);
  return (dirNameLength==string::npos)?string():path.substr(0, dirNameLength+1);
}
bool CmdLineOptions::attachLineAnchors()const{
    return opt_attach_line_anchors;
}
bool CmdLineOptions::showThemes()const{
    return opt_show_themes;
}
bool CmdLineOptions::showLangdefs()const{
    return opt_show_langdefs;
}
bool CmdLineOptions::outDirGiven()const{
    return !outFilename.empty();
}
bool CmdLineOptions::fopCompatible() const {
   return opt_xslfo_fop;
}
bool CmdLineOptions::replaceQuotes() const {
   return opt_replacequotes;
}
bool CmdLineOptions::getFlag( const string& paramVal){
   return StringTools::lowerCase(paramVal)=="true";
}
bool CmdLineOptions::formattingEnabled(){
    return !indentScheme.empty();
}
bool CmdLineOptions::dataDirGiven()const {
  return !dataDir.empty();
}
bool CmdLineOptions::additionalDataDirGiven()const {
  return !additionalDataDir.empty();
}
const string &CmdLineOptions::getDataDir() const {
 return dataDir;
}
const string &CmdLineOptions::getIndentScheme() const {
 return indentScheme;
}
const string &CmdLineOptions::getAdditionalDataDir()const{
 return additionalDataDir;
}
const string &CmdLineOptions::getLanguage() const {
 return language;
}
const string&CmdLineOptions::getCharSet() const{
    return charset;
}
bool CmdLineOptions::printIndexFile() const{
 return opt_printindex && (outputType==highlight::HTML ||
                           outputType==highlight::XHTML);
}
bool CmdLineOptions::printProgress() const{
 return opt_print_progress;
}
bool CmdLineOptions::fillLineNrZeroes() const{
  return opt_fill_zeroes;
}
bool CmdLineOptions::syntaxGiven() const{
  return opt_language;
}
bool CmdLineOptions::omitEncodingName() const{
    return StringTools::lowerCase(charset)=="none";
}
bool CmdLineOptions::forceOutput() const{
    return opt_force_output;
}
const string  CmdLineOptions::getHelpLang()const{
  return helpLang+".help";
}
highlight::WrapMode CmdLineOptions::getWrappingStyle() const {
  return wrappingStyle;
}
const vector <string> & CmdLineOptions::getInputFileNames() const{
  return inputFileNames;
}
void CmdLineOptions::readDirectory(const string & wildcard){
  // get matching files, use  recursive search
  bool directoryOK=Platform::getDirectoryEntries(inputFileNames, wildcard, true);
  if (!directoryOK)
    {
       cerr << "highlight: No files matched the pattern \""
            << wildcard << "\"."<< endl;
    }
}
void CmdLineOptions::loadConfigurationFile()
{
  #ifndef _WIN32
    #ifdef CONFIG_FILE_PATH
       configFilePath=CONFIG_FILE_PATH;
    #else
      char* homeEnv=getenv("HOME");
      if (homeEnv==NULL) return;
      configFilePath=string(homeEnv)+"/.highlightrc";
    #endif
  #else
    configFilePath = Platform::getAppPath() + "highlight.conf";
  #endif
  ConfigurationReader presets(configFilePath);

  if (presets.found())
    {
      string paramVal;
      configFileRead=true;

      styleOutFilename = presets.getParameter(OPT_STYLE_OUT);
      styleInFilename = presets.getParameter(OPT_STYLE_IN);
      styleName = presets.getParameter(OPT_STYLE);
      opt_style = !styleName.empty();
      language = presets.getParameter(OPT_SYNTAX);
      opt_language = !language.empty();
      numberSpaces = StringTools::str2int(presets.getParameter(OPT_DELTABS));
      indentScheme = presets.getParameter(OPT_FORMATSTYLE);

      paramVal = presets.getParameter(OPT_DATADIR);
      if (!paramVal.empty()) {
         dataDir=validateDirPath( paramVal);
      }
      paramVal = presets.getParameter(OPT_ADDDATADIR);
      if (!paramVal.empty()) {
        additionalDataDir=validateDirPath(paramVal);
      }
      paramVal = presets.getParameter(OPT_OUTDIR);
      if (!paramVal.empty()) {
        outDirectory=validateDirPath(paramVal);
      }
      paramVal = presets.getParameter(OPT_ENCODING);
      if (!paramVal.empty()) {
          charset=paramVal;
      }

      opt_include_style=getFlag(presets.getParameter(OPT_INC_STYLE));
      opt_verbose=getFlag(presets.getParameter(OPT_VERBOSE));
      opt_linenumbers=getFlag(presets.getParameter(OPT_LINENO));
      opt_fragment=getFlag(presets.getParameter(OPT_FRAGMENT));
      opt_attach_line_anchors=getFlag(presets.getParameter(OPT_ANCHORS));
      opt_printindex=getFlag(presets.getParameter(OPT_INDEXFILE));
      opt_quiet=getFlag(presets.getParameter(OPT_QUIET));
      opt_xslfo_fop=getFlag(presets.getParameter(OPT_FOP));
      opt_replacequotes=getFlag(presets.getParameter(OPT_REPLACE_QUOTES));
      opt_print_progress=getFlag(presets.getParameter(OPT_PROGRESSBAR));
      opt_fill_zeroes=getFlag(presets.getParameter(OPT_FILLZEROES));

      if (getFlag(presets.getParameter(OPT_WRAP))) {
          wrappingStyle=highlight::WRAP_DEFAULT;
      }
      if (getFlag(presets.getParameter(OPT_WRAPSIMPLE))) {
          wrappingStyle=highlight::WRAP_SIMPLE;
      }
      if (getFlag(presets.getParameter(OPT_XHTML))) {
          outputType=highlight::XHTML;
      } else if (getFlag(presets.getParameter(OPT_RTF))) {
          outputType=highlight::RTF;
      } else if (getFlag(presets.getParameter(OPT_TEX))) {
          outputType=highlight::TEX;
      } else if (getFlag(presets.getParameter(OPT_LATEX))) {
          outputType=highlight::LATEX;
      } else if (getFlag(presets.getParameter(OPT_XSLFO))) {
          outputType=highlight::XSLFO;
      } else if (getFlag(presets.getParameter(OPT_ANSI))) {
          outputType=highlight::ANSI;
      } else if (getFlag(presets.getParameter(OPT_XML))) {
            outputType=highlight::XML;
      }
    }
}

string CmdLineOptions::validateDirPath(const string & path){
   return (path[path.length()-1] !=Platform::pathSeparator)?
              path+Platform::pathSeparator : path;
}

highlight::OutputType CmdLineOptions::getOutputType() const {
    return outputType;
}

void CmdLineOptions::printDeprecatedWarning(const char *oldOption, const char *newOption){
    cerr << "Warning: Long option \""<<oldOption << "\" is DEPRECATED.";
    cerr << " Use \""<<newOption << "\" instead.\n";
}
/***************************************************************************
                          cmdlineoptions.h  -  description
                             -------------------
    begin                : Sun Nov 25 2001
    copyright            : (C) 2001 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef CMDLINEOPTIONS_H
#define CMDLINEOPTIONS_H

#include <string>
#include <map>
#include <cstdlib>
#include <iostream>
#include <fstream>

#include "platform_fs.h"
#include "configurationreader.h"
#include "datadir.h"
#include "enums.h"

#ifdef _WIN32
  #include <windows.h>
#endif

// If your system does not know getopt_long, define USE_LOCAL_GETOPT
#if defined(_WIN32) || defined(__SVR4) || defined(__sun__)
  // some compilers don't like redefinitions...
  #ifndef USE_LOCAL_GETOPT
     #define USE_LOCAL_GETOPT
  #endif
#endif

#ifdef USE_LOCAL_GETOPT
  #include "getopt.h"
#else
  #include <getopt.h>
#endif

#define OPT_VERBOSE      "verbose"
#define OPT_INC_STYLE    "include-style"
#define OPT_HELP         "help"
#define OPT_LINENO       "linenumbers"
#define OPT_XHTML        "xhtml"
#define OPT_RTF          "rtf"
#define OPT_TEX          "tex"
#define OPT_LATEX        "latex"
#define OPT_XSLFO        "xsl-fo"
#define OPT_FRAGMENT     "fragment"
#define OPT_ANCHORS      "anchors"
#define OPT_LISTTHEMES   "list-themes"
#define OPT_LISTLANGS    "list-langs"
#define OPT_VERSION      "version"
#define OPT_IN           "input"
#define OPT_OUT          "output"
#define OPT_SYNTAX       "syntax"
#define OPT_STYLE        "style"
#define OPT_STYLE_OUT    "style-outfile"
#define OPT_STYLE_IN     "style-infile"

#define OPT_DELTABS      "replace-tabs"
#define OPT_BATCHREC     "batch-recursive"
#define OPT_OUTDIR       "outdir"
#define OPT_FORMATSTYLE  "format-style"
#define OPT_DATADIR      "data-dir"
#define OPT_ADDDATADIR   "add-data-dir"
#define OPT_INDEXFILE    "print-index"
#define OPT_HELPINT      "help-int"
#define OPT_WRAP         "wrap"
#define OPT_WRAPSIMPLE   "wrap-simple"
#define OPT_QUIET        "quiet"
#define OPT_REPLACE_QUOTES  "replace-quotes"
#define OPT_FOP          "fop-compatible"
#define OPT_PROGRESSBAR  "progress"
#define OPT_FILLZEROES   "zeroes"
#define OPT_ANSI         "ansi"
#define OPT_XML          "xml"
#define OPT_ENCODING     "encoding"
#define OPT_FORCE_OUTPUT "force"

#define S_OPT_ANSI       'A'
#define S_OPT_OUT        'o'
#define S_OPT_IN         'i'
#define S_OPT_SYNTAX     'S'
#define S_OPT_VERBOSE    'v'
#define S_OPT_INC_STYLE  'I'
#define S_OPT_HELP       'h'
#define S_OPT_HELPINT    'H'
#define S_OPT_LINENO     'l'
#define S_OPT_STYLE      's'
#define S_OPT_STYLE_OUT  'c'
#define S_OPT_STYLE_IN   'e'
#define S_OPT_DELTABS    't'
#define S_OPT_XHTML      'X'
#define S_OPT_RTF        'R'
#define S_OPT_TEX        'T'
#define S_OPT_LATEX      'L'
#define S_OPT_XSLFO      'Y'
#define S_OPT_XML        'Z'
#define S_OPT_BATCHREC   'B'
#define S_OPT_FRAGMENT   'f'
#define S_OPT_ANCHORS    'a'
#define S_OPT_LISTTHEMES 'w'
#define S_OPT_LISTLANGS  'p'
#define S_OPT_OUTDIR     'O'

#define S_OPT_FORMATSTYLE 'F'
#define S_OPT_DATADIR     'D'
#define S_OPT_ADDDATADIR  'E'
#define S_OPT_INDEXFILE   'C'
#define S_OPT_WRAP        'W'
#define S_OPT_WRAPSIMPLE  'V'
#define S_OPT_QUIET       'q'
#define S_OPT_FOP            'g'
#define S_OPT_REPLACE_QUOTES 'r'
#define S_OPT_VERSION        'Q'
#define S_OPT_PROGRESSBAR    'P'
#define S_OPT_FILLZEROES     'z'
#define S_OPT_ENCODING       'u'

// deprecated:
#define OPT_CSSOUT       "css-outfile"
#define OPT_CSSIN        "css-infile"
#define OPT_INC_CSS      "include-css"


#define S_OPTIONS_STRING "o:i:S:B:O:s:c:e:t:u:F:D:H:E:afghlvwpqrzACILYRTZXUV::W::P"

using namespace std;

/**Command line options*/

class CmdLineOptions
  {
  public:

    /**Constructor
     \param argc Argument count
     \param argv Argument strings
    */
    CmdLineOptions(int argc, char *argv[]);
    ~CmdLineOptions();

    /** \return Single output file name*/
    const string &getSingleOutFilename();

    /** \return Single input file name*/
    const string &getSingleInFilename() const;

    /** \return Output directory*/
    const string& getOutDirectory() ;

    /** \return Style output file name*/
    const string getStyleOutFilename() const;

    /** \return Style input file name*/
    const string&getStyleInFilename() const;

    /** \return Char set*/
    const string&getCharSet() const;

    /** \return Number of spaces to replace a tab*/
    int getNumberSpaces() const;

    /** \return True if version information should be printed*/
    bool printVersion() const;

   /** \return True if help information should be printed*/
    bool printHelp() const;

    /** \return True if debug information should be printed*/
    bool printDebugInfo()const;

    /** \return True if Style definition should be included in output*/
    bool includeStyleDef() const;

    /** \return True if line numbers should be printed*/
    bool printLineNumbers() const;

    /** \return colour theme name */
    string getStyleName()const ;

    /** gibt true zurck, falls deutsche Hilfe ausgegeben werden soll */
    int helpLanguage() const;

    /** \return True if batch mode is active*/
    bool enableBatchMode() const;

    /** \return True if output shluld be fragmented*/
    bool fragmentOutput() const;

    /** \return output file suffix */
    string getOutFileSuffix() const;

    /** \return True if anchors should be attached to line numbers*/
    bool attachLineAnchors() const;

    /** \return True if list of installed themes should be printed*/
    bool showThemes() const;

    /** \return True if list of installed language definitions should be printed*/
    bool showLangdefs() const;

    /** \return True if loutput directory is given*/
    bool outDirGiven() const;

    /** \return True if refomatting is enabled*/
    bool formattingEnabled();

    /** \return True if a new data directory is given*/
    bool dataDirGiven()const;

    /** \return True if an additional data directory is given*/
    bool additionalDataDirGiven()const;

    /** \return True if index file should be printed*/
    bool printIndexFile() const;

    /** \return True if quotes should be replaced by /dq in LaTeX*/
    bool replaceQuotes() const;

    /** \return Data directory*/
    const string &getDataDir()const;

    /** \return Additional data directory*/
    const string &getAdditionalDataDir()const;

    /** \return True if language syntax is given*/
    bool syntaxGiven() const;

    /** \return True if quiet mode is active*/
    bool quietMode() const;

    /** \return True if XSL-FO output should be FOP compatible*/
    bool fopCompatible() const;

    /** \return True if progress bar should be printed in batch mode */
    bool printProgress() const;

    /** \return True if line numbers are filled with leading zeroes */
    bool fillLineNrZeroes() const;

    /** \return name of help message file*/
    const string getHelpLang() const;

    /** \return programming language */
    const string &getLanguage()const ;

    /** \return Wrapping style*/
    highlight::WrapMode getWrappingStyle() const;

    /** \return List of input file names*/
    const vector <string> & getInputFileNames() const;

    /** \return Name of indentation scheme file */
    const string &getIndentScheme() const;

    /** \return Output file format */
    highlight::OutputType getOutputType() const;

    /** \return True if chosen output format supports referenced style files */
    bool formatSupportsExtStyle();

    /** \return True if style output path was defined by user*/
    bool styleOutPathDefined() const{
        return opt_stylepath_explicit;
    }

    /** \return True if encoding nasme should be omitted in output*/
    bool omitEncodingName() const;

    /** \return True if output should be generated if languege type is unknown*/
    bool forceOutput() const;

  private:

    int numberSpaces;   // number of spaces which replace a tab
    highlight::WrapMode wrappingStyle; // line wrapping mode
    highlight::OutputType outputType;

    // name of single output file
    string outFilename,
    // output directory
    outDirectory,
    // programming language which will be loaded
    language,
    // name of colour theme
    styleName,
    // name of external style file
    styleOutFilename,
    // name of file to be included in external style file
    styleInFilename,
    // used to define data directories at runtime
    dataDir, additionalDataDir;
    // name of indenation scheme
    string indentScheme;

    bool opt_language;
    bool opt_include_style;
    bool opt_help;
    bool opt_version ;
    bool opt_verbose;
    bool opt_linenumbers;
    bool opt_style;
    bool opt_batch_mode;
    bool opt_fragment;
    bool opt_attach_line_anchors;
    bool opt_show_themes;
    bool opt_show_langdefs;
    bool opt_asformat_output;
    bool opt_printindex;
    bool opt_quiet;
    bool opt_xslfo_fop;
    bool opt_replacequotes;
    bool opt_print_progress;
    bool opt_fill_zeroes;
    bool opt_stylepath_explicit;
    bool opt_force_output;

    bool configFileRead;

    string helpLang, charset;
    string configFilePath;

    // list of all input file names
    vector <string> inputFileNames;

    /** load highlight configuration file */
    void loadConfigurationFile();

    /** \return file suffix */
    string getFileSuffix( const string & fileName) const;

    /** \return directory name of path */
    string getDirName( const string & path);

    /** get all entries in the directory defined by wildcard */
    void readDirectory(const string & wildcard);

    /** \return Boolean value of paramVal */
    bool getFlag(const string& paramVal);

    /** \return Valid path name */
    string validateDirPath(const string & path);

    void printDeprecatedWarning(const char *oldOption, const char *newOption);
  };

#endif
/***************************************************************************
                          codeparser.cpp  -  description
                             -------------------
    begin                : Die Jul 9 2002
    copyright            : (C) 2002 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "codegenerator.h"

#include "htmlgenerator.h"
#include "xhtmlgenerator.h"
#include "rtfgenerator.h"
#include "latexgenerator.h"
#include "texgenerator.h"
#include "xslfogenerator.h"
#include "xmlgenerator.h"
#ifndef __WXMSW__
  #include "ansigenerator.h"
#endif


using namespace std;

namespace highlight {

CodeGenerator* CodeGenerator::generator=NULL;

CodeGenerator* CodeGenerator::getInstance(OutputType type,
                                          const string& styleInfoPath,
                                          const string& styleInPath,
                                          const string& styleOutPath,
                                          const string& encoding,
                                          bool includeStyle,
                                          bool attachAnchors,
                                          bool replaceQuotes,
                                          bool fopCompatible,
                                          int numSpaces,
                                          WrapMode lineWrappingMode,
                                          bool ln,
                                          bool lnz,
                                          bool fragment,
                                          bool omitEncoding
                                          ) {
  if (generator==NULL){
     switch (type){
      case TEX:
        generator = new TexGenerator (styleInfoPath);
        break;
      case LATEX:
        generator = new LatexGenerator(styleInfoPath, replaceQuotes);
        break;
      case RTF:
        generator = new RtfGenerator (styleInfoPath);
        break;
      case XSLFO:
          generator = new XslFoGenerator(styleInfoPath, encoding, omitEncoding,
                                         fopCompatible);
        break;
      case XML:
          generator = new XmlGenerator(styleInfoPath,encoding, omitEncoding);
        break;
      case XHTML:
          generator = new XHtmlGenerator(styleInfoPath, encoding, omitEncoding,
                                         attachAnchors);
          break;
      #ifndef __WXMSW__
      case ANSI:
        generator = new AnsiGenerator (styleInfoPath);
        break;
      #endif
      default:
          generator = new HtmlGenerator(styleInfoPath, encoding, omitEncoding,
                                        attachAnchors);
     }
  }
  generator->setType(type);
  generator->setStyleInputPath(styleInPath);
  generator->setStyleOutputPath(styleOutPath);
  generator->setIncludeStyle(includeStyle);
  generator->setPrintLineNumbers(ln);
  generator->setPrintZeroes(lnz);
  generator->setFragmentCode(fragment);
  generator->setPreformatting(lineWrappingMode,
                             (generator->getPrintLineNumbers()) ?
                              MAX_LINE__WIDTH - LINE_NUMBER_WIDTH : MAX_LINE__WIDTH,
                              numSpaces );
  return generator;
}

void CodeGenerator::deleteInstance(){
  delete generator;
  generator=NULL;
}


CodeGenerator::CodeGenerator():
    in(NULL),
    out(NULL),
    maskWs(false),
    excludeWs(false),
    fragmentOutput(false),
    showLineNumbers (false),
    lineNumberFillZeroes(false),
    lineNumber(0),
    includeStyleDef(false),
    lineIndex(0),
    formatter(NULL),
    preFormatter(NULL),
    formattingEnabled(false),
    formattingPossible(false),
    outputType(highlight::HTML)
{}

CodeGenerator::CodeGenerator(const string &colourTheme)
   :in(NULL),
    out(NULL),
    maskWs(false),
    excludeWs(false),
    fragmentOutput(false),
    showLineNumbers (false),
    lineNumberFillZeroes(false),
    lineNumber(0),
    includeStyleDef(false),
    stylePath(colourTheme),
    lineIndex(0),
    formatter(NULL),
    preFormatter(NULL),
    formattingEnabled(false),
    formattingPossible(false),
    outputType(highlight::HTML)
{
  line.reserve(100);
  docStyle.load(stylePath);
}

CodeGenerator::~CodeGenerator()
{
  delete preFormatter;
  delete formatter;
}


/** Getter and Setter*/

void CodeGenerator::setPrintLineNumbers(bool flag){
  showLineNumbers=flag;
}

bool CodeGenerator::getPrintLineNumbers(){
  return showLineNumbers;
}

void CodeGenerator::setPrintZeroes(bool flag){
  lineNumberFillZeroes=flag;
}

bool CodeGenerator::getPrintZeroes(){
  return lineNumberFillZeroes;
}

void CodeGenerator::setFragmentCode(bool flag){
  fragmentOutput=flag;
}

void CodeGenerator::setIncludeStyle(bool flag){
    includeStyleDef = flag;
}

void CodeGenerator::setStyleInputPath(const string& path){
    styleInputPath = path;
}
void CodeGenerator::setStyleOutputPath(const string& path){
    styleOutputPath = path;
}

const string&  CodeGenerator::getStyleInputPath(){
    return styleInputPath;
}
const string&  CodeGenerator::getStyleOutputPath(){
    return styleOutputPath;
}


bool CodeGenerator::getFragmentCode(){
  return fragmentOutput;
}

void CodeGenerator::setStyleName(const string& s){
  stylePath=s;
}

void CodeGenerator::setType(OutputType t){
    outputType = t;
}

const string& CodeGenerator::getStyleName(){
  return stylePath;
}

bool CodeGenerator::formattingDisabled(){
  return !formattingEnabled;
}

bool CodeGenerator::formattingIsPossible(){
  return formattingPossible;
}

void CodeGenerator::setPreformatting(WrapMode lineWrappingStyle,
                                     unsigned int lineLength,
                                     int numberSpaces ){
  bool enableWrap = lineWrappingStyle!=WRAP_DISABLED;
  bool replaceTabs = numberSpaces > 0;
  if (enableWrap || replaceTabs)  {
    preFormatter=new PreFormatter(enableWrap, replaceTabs);
    if (enableWrap)
      preFormatter->setWrappingProperties(lineLength, lineWrappingStyle==WRAP_DEFAULT);
    if (replaceTabs)
      preFormatter->setNumberSpaces(numberSpaces);
  }
}

/*
WrapMode CodeGenerator::getLineWrapping(){
  if (preFormatter==NULL) return WRAP_DISABLED;
  return (preFormatter->indentCode()?WRAP_DEFAULT:WRAP_SIMPLE);
}
*/
LanguageDefinition &CodeGenerator::getLanguage(){
  return langInfo;
}

void CodeGenerator::reset()
{
  lineIndex = lineNumber = 0;
  line.clear();
}


/** sucht vorwaerts ab Position searchPos Ziffer in s und liefert Integerwert
der gefundenen Zahl zurueck.
Im SymbolString stehen die den einzelnen Symbolen zugeordneten Konstanten
immer HINTER diesen Symbolen*/
State CodeGenerator::getState(const string &s, unsigned int searchPos)
{
  unsigned int i= searchPos+1, result=0;

  // nach Ziffer in s suchen
  do {
      ++i;
  } while ((i<s.length()) && !isdigit(s[i])) ;

  // Zahl zusammensetzen
  while ((i<s.length()) && isdigit(s[i])){
    result = result *10 + (s[i]-'0');
    ++i;
  }
  return ((result)? (State)result:_UNKNOWN);
}

string CodeGenerator::getIdentifier()
{
  --lineIndex;
  unsigned int startPos=lineIndex;
  char c= line[lineIndex];

  while (    ( lineIndex < line.length()
          && (   StringTools::isAlpha(c)
              || isdigit(c))
              || isAllowedChar(c))
          )
    {
      ++lineIndex;
      c= line[lineIndex];
    }
  return string(line, startPos, lineIndex - startPos);
}

string CodeGenerator::getNumber()
{
  --lineIndex;
  unsigned int startPos=lineIndex;
  char c=line[lineIndex];

  while ( lineIndex < line.length() && (
          isdigit(c)
          // don't highlight methods applied on numbers as part of the number
          // i.e. Ruby: 3.xxx()
          || (c == '.' && isdigit(line[lineIndex+1]))
          // '-' is accepted as first character
          || (c == '-' && lineIndex == startPos)
          || (StringTools::isAlpha(c) && line[lineIndex-1]=='0')
          || (isxdigit(c) || c=='L' || c=='U' || c=='l' || c=='u') ))
    {
      ++lineIndex;
      c= line[lineIndex];
    }
  return string(line,startPos, lineIndex-startPos);
}

unsigned int CodeGenerator::getLineNumber()
{
  return lineNumber;
}

bool CodeGenerator::readNewLine(string &newLine){
  bool eof;
  terminatingChar=newLine[lineIndex-1];
  if (formattingPossible && formattingEnabled)
   {
     eof=!formatter->hasMoreLines();
      if (!eof)
       {
         newLine = formatter->nextLine();
       }
   }
   else  // reformatting not enabled
    {
      eof = ! getline( *in, newLine);
    }
   return eof;
}

unsigned char CodeGenerator::getInputChar()
{
  bool eol = lineIndex == line.length();

  if (eol) {
    bool eof=false;
    if (preFormatter!=NULL){
       if (!preFormatter->hasMoreLines()) {
          eof=readNewLine(line);
          preFormatter->setLine(line);
       }
       line = preFormatter->getNextLine();
    } else {
      eof=readNewLine(line);
    }
    lineIndex=0;
    ++lineNumber;
    line=StringTools::trimRight(line);
    return (eof)?'\0':'\n';
  }
  return line[lineIndex++];
}

State CodeGenerator::getCurrentState (bool lastStateWasNumber)
{
  unsigned char c;

  if (token.length()==0) {
    c=getInputChar();
  }  else {
    lineIndex-= (token.length()-1);
    c=token[0];
  }
  if (c=='\n'){
    return _EOL;   // End of line
  }

  if (c=='\0') {
    return _EOF;   // End of file
  }

  if (isspace(c)) {
      token= c;
      return _WS;
    }

  // numbers have to be searched before using the symbolstring,
  // as numbers are part of this string
  if (isdigit(c)
      // recognize floats like .5
      || (c=='.' && isdigit(line[lineIndex]))
      // test if '-' belongs to a term like "1-2"
      || ((c == '-')
          && (!lastStateWasNumber)
          && isdigit(StringTools::getNextNonWs(line, lineIndex))) )
    {
      token = getNumber();
      return NUMBER;
    }
  unsigned int symbolLength;
  size_t symbolPos;
  bool found=false;
  string symbols=langInfo.getSymbolString();

  symbolPos = symbols.find(c);
  // search symbols (comment delimiters, directives etc.)
  // before keywords, because alphabetic chars may be part of symbols, too
  while ((symbolPos!= string::npos) && (!found))
    {
      symbolLength=symbols.find(' ', symbolPos)-symbolPos;
      token = symbols.substr(symbolPos, symbolLength);

      // TODO Ruby =ende, =end bugfix (whitespace after symbol needs to be checked)

      // Abfrage nach Leerzeichen in SymbolString verhindert falsches
      // Erkennen von Symbolteilen:
      if (lineIndex && token == line.substr(lineIndex-1, symbolLength)
                    && isspace(symbols[symbolPos-1]) ) {
         found = true;
         lineIndex += (symbolLength-1);
      } else {
        symbolPos = symbols.find_first_not_of(' ',symbols.find(' ',symbolPos));
      }
    }

  // dirty workaround stuff in here
  if (found) {
    State foundState = getState(symbols, symbolPos);

    // get the current keyword class id to apply the corresponding formatting style
    if (foundState==KEYWORD_BEGIN || foundState==TAG_BEGIN ) {
      currentKeywordClass=langInfo.getDelimPrefixClassID(token);
    }

    // Full line quotes must start in coloumn 1 (Fortran 77)
    if (langInfo.isFullLineComment() && foundState==SL_COMMENT){
      if (lineIndex==1) {
        return SL_COMMENT;
      }
    }
    // VHDL Workaround: distinguish string delimiters and event markers
    // (same eymbol: ')
    else if (langInfo.isVHDL()
             && foundState==STRING && currentState!=STRING
             && lineIndex > 1
             &&(isdigit(line[lineIndex-2]) || isalpha(line[lineIndex-2]))){
        c=line[lineIndex-1];
        // do not return, continue search...
    }  else {
        return foundState;
    }
  }

  // Alphanumerisches Token parsen und als Keyword oder Type erkennen
  if (StringTools::isAlpha(c) || langInfo.isPrefix(c) || isAllowedChar(c))
    {
      if (langInfo.isPrefix(c)){
        token = c;
        ++lineIndex;
        token += getIdentifier();
      } else {
        token = getIdentifier();
      }
      string reservedWord=(langInfo.isIgnoreCase()) ?
                          StringTools::lowerCase(token):token;
      currentKeywordClass=langInfo.isKeyword(reservedWord);
      return (currentKeywordClass) ? KEYWORD : STANDARD;
    }

  // Character not referring to any state
  token = c;
  return STANDARD;
}

string CodeGenerator::maskString(const string & s)
{
  ostringstream ss;
  for (unsigned int i=0;i< s.length();i++){
      ss << maskCharacter(s[i]);
  }
  return ss.str();
}

void CodeGenerator::printMaskedToken(bool flushWhiteSpace)
{
  if(flushWhiteSpace) flushWs();
  *out << maskString(token);
  token.clear();
}

bool CodeGenerator::isAllowedChar(char c)
{
  return ( langInfo.getAllowedChars().find(c)!= string::npos);
}

bool CodeGenerator::styleFound(){
  return docStyle.found();
}

bool CodeGenerator::printIndexFile(const vector<string> &fileList,
                                   const string &outPath){
  return true;
}

bool CodeGenerator::initIndentationScheme(const string &schemePath){

  if (formatter!=NULL){
    return true;
  }

  ConfigurationReader indentScheme(schemePath);
  if (indentScheme.found()){
    if (formatter==NULL) {
       formatter=new astyle::ASFormatter();

       string brackets=indentScheme.getParameter("brackets");
       if (!brackets.empty()){
         // Break brackets from pre-block code (i.e. ANSI C/C++ style).
         if (brackets=="break"){
           formatter->setBracketFormatMode(astyle::BREAK_MODE);
         }
         //Attach brackets to pre-block code (i.e. Java/K&R style).
         else if (brackets=="attach"){
           formatter->setBracketFormatMode(astyle::ATTACH_MODE);
         }
         // Break definition-block brackets and attach command-block brackets.
         else if (brackets=="linux"){
           formatter->setBracketFormatMode(astyle::BDAC_MODE);
         }
         // Break brackets before closing headers (e.g. 'else', 'catch', ..).
         // Should be appended to --brackets=attach or --brackets=linux.
         else if (brackets=="break-closing-headers"){
           formatter->setBreakClosingHeaderBracketsMode(true);
         }
       }

       string pad=indentScheme.getParameter("pad");
       if (!pad.empty()){
         //Insert space paddings around parenthesies only.
         if (pad=="paren"){
           formatter->setParenthesisPaddingMode(true);
         }
         // Insert space paddings around operators only.
         else if (pad=="oper"){
           formatter->setOperatorPaddingMode(true);
         }
         //Insert space paddings around operators AND parenthesies.
         else if (pad=="all"){
           formatter->setOperatorPaddingMode(true);
           formatter->setParenthesisPaddingMode(true);
         }
       }

       string oneLine=indentScheme.getParameter("one-line");
       if (!oneLine.empty()){
         // Don't break one-line blocks.
         if (oneLine=="keep-blocks"){
           formatter->setBreakOneLineBlocksMode(false);
         }
         // Don't break complex statements and multiple statements residing in a
         // single line.
         else if (oneLine=="keep-statements"){
           formatter->setSingleStatementsMode(false);
         }
       }

       // Insert empty lines around unrelated blocks, labels, classes, ...
       string breakBlocks=indentScheme.getParameter("break-blocks");
       if (!breakBlocks.empty()){
         if (breakBlocks=="all"){
           //Like --break-blocks, except also insert empty lines around closing
           //headers (e.g. 'else', 'catch', ...).
           formatter->setBreakClosingHeaderBlocksMode(true);
         }
         formatter->setBreakBlocksMode(true);
       }
       string trueVal="true";

       // Other options...

       //Indent using # spaces per indent. Not specifying # will result in a
       //default of 4 spaces per indent.
       string indentSpaces=indentScheme.getParameter("indent-spaces");

       // Indent a minimal # spaces in a continuous conditional belonging to a
       //conditional header.
       string minConditionalIndent=indentScheme.getParameter("min-conditional-indent");

       // Indent a maximal # spaces in a continuous statement, relatively to the
       // previous line.
       string maxInStatementIndent=indentScheme.getParameter("max-instatement-indent");

       // Add extra indentation to '{' and '}' block brackets.
       string indentBrackets=indentScheme.getParameter("indent-brackets");

       // Add extra indentation entire blocks (including brackets).
       string indentBlocks=indentScheme.getParameter("indent-blocks");

       // Indent the contents of namespace blocks.
       string indentNamespaces=indentScheme.getParameter("indent-namespaces");

       // Indent 'class' blocks, so that the inner 'public:','protected:' and
       // 'private: headers are indented inrelation to the class block.
       string indentClasses=indentScheme.getParameter("indent-classes");

       // Indent 'switch' blocks, so that the inner 'case XXX:' headers are
       // indented in relation to the switch block.
       string indentSwitches=indentScheme.getParameter("indent-switches");

       // Indent 'case XXX:' lines, so that they are flush with their bodies..
       string indentCases=indentScheme.getParameter("indent-cases");

       // Indent labels so that they appear one indent less than the current
       // indentation level, rather than being    flushed completely to the left
       // (which is the default).
       string indentLabels=indentScheme.getParameter("indent-labels");

       // Indent multi-line #define statements
       string indentPreprocessor=indentScheme.getParameter("indent-preprocessor");

       // Break 'else if()' statements into two different lines.
       string breakElseIfs = indentScheme.getParameter("break-elseifs");

       string javaStyle = indentScheme.getParameter("java-style");

       // default values in ASBeautifier are false, it is ok to set them false
       // if parameter does not exist in scheme file
       formatter->setBracketIndent(indentBrackets==trueVal);
       formatter->setBlockIndent(indentBlocks==trueVal);
       formatter->setNamespaceIndent(indentNamespaces==trueVal);
       formatter->setClassIndent(indentClasses==trueVal);
       formatter->setSwitchIndent(indentSwitches==trueVal);
       formatter->setCaseIndent(indentCases==trueVal);
       formatter->setLabelIndent(indentLabels==trueVal);
       formatter->setPreprocessorIndent(indentPreprocessor==trueVal);
       formatter->setBreakElseIfsMode(breakElseIfs==trueVal);

       if (javaStyle==trueVal){
         formatter->setJavaStyle();
       }

       if (!indentSpaces.empty()){
         formatter->setSpaceIndentation(StringTools::str2int(indentSpaces));
       }
       if (!minConditionalIndent.empty()){
         formatter->setMinConditionalIndentLength(
                      StringTools::str2int(minConditionalIndent));
       }
       if (!maxInStatementIndent.empty()){
         formatter->setMinConditionalIndentLength(
                      StringTools::str2int(maxInStatementIndent));
       }
    }
    formattingEnabled=(formatter != NULL);
    return true;
  } else {
    return false;
  }
}

LoadResult CodeGenerator::initLanguage(const string& langDefPath){
  bool reloadNecessary= langInfo.needsReload(langDefPath);
  if (reloadNecessary){
    bool failure = !langInfo.load(langDefPath);

    if (failure) {
      return LOAD_FAILED;
    }

    formattingPossible=langInfo.enableReformatting();

    if (styleTagOpen.size()>NUMBER_BUILTIN_STYLES){
       // remove dynamic keyword tag delimiters of the old language definition
       vector<string>::iterator keyStyleOpenBegin =
           styleTagOpen.begin() + NUMBER_BUILTIN_STYLES;
       vector<string>::iterator keyStyleCloseBegin =
           styleTagClose.begin()+ NUMBER_BUILTIN_STYLES;
       styleTagOpen.erase(keyStyleOpenBegin, styleTagOpen.end());
       styleTagClose.erase(keyStyleCloseBegin, styleTagClose.end());
    }
    // add new keyword delimiters
    for (unsigned int i=0;i< langInfo.getKeywordClasses().size(); i++){
      styleTagOpen.push_back(getMatchingOpenTag(i));
      styleTagClose.push_back(getMatchingCloseTag(i));
    }
  }
  return (reloadNecessary) ? LOAD_NEW : LOAD_NONE;
}

ParseError CodeGenerator::printOutput (const string & inFileName,
                                       const string &outFileName)
{
  if (!docStyle.found()) {
    return BAD_STYLE;
  }
  reset();

  ParseError error=PARSE_OK;

  in = (inFileName.empty()? &cin :new ifstream (inFileName.c_str()));
  if (!in->fail()) {
    out = (outFileName.empty()? &cout :new ofstream (outFileName.c_str()));
    if ( out->fail()) {
      error=BAD_OUTPUT;
    }
  }

  if ( in->fail()){
     error=BAD_INPUT;
  }

  if (error==PARSE_OK) {
    if (formatter != NULL){
       formatter->init(new astyle::ASStreamIterator(in));
    }
    if (! fragmentOutput){
      *out << getHeader(inFileName);
    }
    printBody();
    if (! fragmentOutput){
      *out << getFooter();
    }
  }

  if (!outFileName.empty()){
    delete out; out=NULL;
  }
  if (!inFileName.empty()) {
    delete in; in=NULL;
  }
  return error;
}


unsigned int CodeGenerator::getStyleID(State s, unsigned int kwClassID){
    if (s==KEYWORD && kwClassID){
      return NUMBER_BUILTIN_STYLES + kwClassID-1;
  }
  return (unsigned int) s ;
}

void CodeGenerator::closeTag(State s){
  *out << styleTagClose[(unsigned int)s];
  flushWs();
  currentState=_UNKNOWN;
}

void CodeGenerator::openTag(State s){
  *out << styleTagOpen[(unsigned int)s];
  currentState=s;
}

void CodeGenerator::closeKWTag(unsigned int kwClassID){
    *out << styleTagClose[getStyleID(KEYWORD, kwClassID)];

    flushWs();
    currentState=_UNKNOWN;
}

void CodeGenerator::openKWTag(unsigned int kwClassID){
    *out << styleTagOpen[getStyleID(KEYWORD, kwClassID)];
    currentState=KEYWORD;
}


///////////////////////////////////////////////////////////////////////////////

void CodeGenerator::processRootState()
{
  if (langInfo.highlightingDisabled()){
     string line;
     while (getline(*in, line)){
       *out << maskString(line) << getNewLine();
     }
     *out << flush;
     return;
  }

  State state=STANDARD;

  bool eof=false,
       firstLine=true; // avoid newline before printing the first output line
  openTag(STANDARD);
  do {
    // determine next state
    state= getCurrentState(state==NUMBER);
    // handle current state
    switch(state)
      {
      case KEYWORD:
      case KEYWORD_BEGIN:
        closeTag(STANDARD);
        eof=processKeywordState(state);
        openTag(STANDARD);
        break;
      case NUMBER:
        closeTag(STANDARD);
        eof=processNumberState();
        openTag(STANDARD);
        break;
      case ML_COMMENT_BEGIN:
        closeTag(STANDARD);
        eof=processMultiLineCommentState();
        openTag(STANDARD);
        break;
      case SL_COMMENT:
        closeTag(STANDARD);
        eof=processSingleLineCommentState();
        openTag(STANDARD);
        break;
      case STRING:
        closeTag(STANDARD);
        eof=processStringState(STANDARD);
        openTag(STANDARD);
        break;
      case DIRECTIVE_LINE:
        closeTag(STANDARD);
        eof=processDirectiveState();
        openTag(STANDARD);
        break;
      case TAG_BEGIN:
        closeTag(STANDARD);
        eof=processTagState();
        openTag(STANDARD);
        break;
      case ESC_CHAR:
        if (langInfo.allowExtEscSeq()){
          closeTag(STANDARD);
          eof=processEscapeCharState();
          openTag(STANDARD);
        } else {
          printMaskedToken();
        }
        break;
      case SYMBOL:
        closeTag(STANDARD);
        eof=processSymbolState();
        openTag(STANDARD);
        break;
      case _EOL:
        insertLineNumber(!firstLine);
        firstLine=false;
        break;
      case _EOF:
        eof=true;
        break;
      case _WS:
        processWsState();
        break;
      default:
        printMaskedToken();
        break;
      }
    }
  while (!eof);
  closeTag(STANDARD);
  *out << getNewLine();
  *out << flush;
}

bool CodeGenerator::processKeywordState(State myState){
  State newState=STANDARD;
  unsigned int myClassID=currentKeywordClass;
  bool eof=false,
       exitState=false;

   openKWTag(myClassID);
  do {
    printMaskedToken(newState!=_WS);
    newState= getCurrentState();
    switch(newState)
      {
      case _WS:
        processWsState();
        break;
      case _EOL:
        insertLineNumber();
        exitState=true;
        break;
      case _EOF:
        eof = true;
        break;
      case KEYWORD_END:
         if (myState==KEYWORD_BEGIN){
           printMaskedToken();
         }
         exitState=true;
        break;
      default:
        exitState=    myState!=KEYWORD_BEGIN
                   &&((myClassID!=currentKeywordClass)||(myState!=newState));
        break;
      }
  } while ((!exitState) && (!eof));

  closeKWTag(myClassID);

  currentKeywordClass=0;
  return eof;
}

bool CodeGenerator::processNumberState(){
  State newState=STANDARD;
  bool eof=false,
       exitState=false;

  openTag(NUMBER);
  do {
    printMaskedToken(newState!=_WS);
    newState= getCurrentState(true);
    switch(newState)
      {
      case _WS:
        processWsState();
        break;
      case _EOL:
        insertLineNumber();
        exitState=true;
        break;
      case _EOF:
        eof = true;
        break;
      default:
        exitState=newState!=NUMBER;
        break;
      }
  } while ((!exitState) && (!eof));

  closeTag(NUMBER);
  return eof;
}

bool CodeGenerator::processMultiLineCommentState()
{
  int commentCount=1;
  State newState=STANDARD;
  bool eof=false, exitState=false;

  openTag(ML_COMMENT_BEGIN);
  do {
    printMaskedToken(newState!=_WS);
    newState= getCurrentState();

    switch(newState)
      {
      case _WS:
        processWsState();
        break;
      case _EOL:
        wsBuffer += styleTagClose[ML_COMMENT_BEGIN];
        insertLineNumber();
        wsBuffer += styleTagOpen[ML_COMMENT_BEGIN];
        break;
      case _EOF:
        eof = true;
        break;
      case ML_COMMENT_BEGIN:
        if (langInfo.allowNestedMLComments()) {
              ++commentCount;
        }
        break;
      case ML_COMMENT_END:
        commentCount--;
        if (!commentCount){
            printMaskedToken();
            exitState=true;
        }
        break;
      default:
        break;
      }
  } while ((!exitState) && (!eof));

  closeTag(ML_COMMENT_BEGIN);
  return eof;
}

bool CodeGenerator::processSingleLineCommentState()
{

  //if ( checkSpecialCmd()) return false;

  State newState=STANDARD;
  bool eof=false, exitState=false;

  openTag(SL_COMMENT);
  do {
    printMaskedToken(newState!=_WS);
    newState= getCurrentState();

    switch(newState)
      {
      case _WS:
        processWsState();
        break;
     case _EOL:
        printMaskedToken();
        insertLineNumber();
        exitState=true;
        break;
      case _EOF:
        eof = true;
        break;
      default:
        break;
      }
  } while ((!exitState) && (!eof));

  closeTag(SL_COMMENT);
  return eof;
}

bool CodeGenerator::processDirectiveState()
{
  State  newState=STANDARD;
  bool eof=false, exitState=false;

  openTag(DIRECTIVE_LINE);
  do {
    printMaskedToken(newState!=_WS);
    newState= getCurrentState();
    switch(newState)
      {
      case _WS:
        processWsState();
        break;
      case DIRECTIVE_LINE_END:
        printMaskedToken();
        exitState=true;
        break;
      case _EOL:
        printMaskedToken();
        exitState=(terminatingChar!=langInfo.getContinuationChar());
        if (!exitState) wsBuffer += styleTagClose[DIRECTIVE_LINE];
        insertLineNumber();
        if (!exitState) wsBuffer += styleTagOpen[DIRECTIVE_LINE];
        break;
      case ML_COMMENT_BEGIN:
        closeTag(DIRECTIVE_LINE);
        eof= processMultiLineCommentState();
        openTag(DIRECTIVE_LINE);
        break;
      case SL_COMMENT:
        closeTag(DIRECTIVE_LINE);
        eof= processSingleLineCommentState();
        openTag(DIRECTIVE_LINE);
        exitState=true;
        break;
      case STRING:
        closeTag(DIRECTIVE_LINE);
        eof=processStringState(DIRECTIVE_LINE);
        openTag(DIRECTIVE_LINE);
        break;
      case _EOF:
        eof = true;
        break;
      default:
        break;
      }
  } while ((!exitState) && (!eof));

  closeTag(DIRECTIVE_LINE);
  return eof;
}

bool CodeGenerator::processStringState(State oldState)
{
  State newState=STANDARD;
  bool eof=false, exitState=false;
  bool returnedFromOtherState=false;
  // Test if character before string open delimiter token equals to the
  // raw string prefix (Example: r" ", r""" """ in Python)
  bool isRawString=
         line[lineIndex-token.length()-1]==langInfo.getRawStringPrefix();

  string openStringDelimiter=token;

  State myState= (oldState==DIRECTIVE_LINE) ? DIRECTIVE_STRING : STRING;
  openTag(myState);
  do {
    // true if last token was an escape char
    if (!returnedFromOtherState) {
         printMaskedToken(newState!=_WS);
    }
    returnedFromOtherState=false;
    newState= getCurrentState();

    switch(newState)
      {
      case _WS:
        processWsState();
        break;
      case _EOL:
        wsBuffer += styleTagClose[myState];
        insertLineNumber();
        wsBuffer += styleTagOpen[myState];
        //exitState=true;
        break;
      case ML_COMMENT_END:
        printMaskedToken();
        break;
      case STRING:
        exitState= openStringDelimiter==token;
        printMaskedToken();
        break;
      case ESC_CHAR:
        if (!isRawString){
           closeTag(myState);
           eof=processEscapeCharState();
           openTag(myState);
           returnedFromOtherState=true;
        }
        break;
      case _EOF:
        eof = true;
        break;
      default:
        printMaskedToken();
        break;
      }
  } while ((!exitState) && (!eof));

  closeTag(myState);
  return eof;
}

bool CodeGenerator::processTagState()
{
  State  newState=STANDARD;
  bool eof=false, exitState=false, returnedFromOtherState=false;
  unsigned int myKeywordClass=currentKeywordClass;

  openTag(KEYWORD);
  do {
    if (!returnedFromOtherState) {
       printMaskedToken(newState!=_WS);
    }
    returnedFromOtherState = false;
    newState= getCurrentState();

    switch(newState)
      {
      case _WS:
        processWsState();
        break;
      case _EOL:
        insertLineNumber();
        exitState=true;
        break;
      case TAG_END:
        printMaskedToken();
        exitState=true;
        break;
      case STRING:
        closeTag(KEYWORD);
        eof=processStringState(KEYWORD);
        currentKeywordClass=myKeywordClass;
        openTag(KEYWORD);
        returnedFromOtherState = true;
        break;
      case ESC_CHAR:
        closeTag(KEYWORD);
        eof=processEscapeCharState();
        currentKeywordClass=myKeywordClass;
        openTag(KEYWORD);
        returnedFromOtherState = true;
        break;
      case NUMBER:
        closeTag(KEYWORD);
        eof=processNumberState();
        currentKeywordClass=myKeywordClass;
        openTag(KEYWORD);
        returnedFromOtherState = true;
        break;
      case _EOF:
        eof = true;
        break;
      default:
        printMaskedToken();
        break;
      }
  } while ((!exitState) && (!eof));

  closeTag(KEYWORD);
  currentKeywordClass=0;

  return eof;
}

bool CodeGenerator::processSymbolState(){

  State newState=STANDARD;
  bool eof=false,
       exitState=false;

  openTag(SYMBOL);
  do {
    printMaskedToken(newState!=_WS);
    newState= getCurrentState(true);
    switch(newState)
      {
     case _WS:
        processWsState();
        break;
      case _EOL:
        insertLineNumber();
        exitState=true;
        break;
      case _EOF:
        eof = true;
        break;
      default:
        exitState=newState!=SYMBOL;
        break;
      }
  } while ((!exitState) && (!eof));

  closeTag(SYMBOL);
  return eof;
}

bool CodeGenerator::processEscapeCharState()
{
  State newState=STANDARD;
  bool eof=false, exitState=false;

  openTag(ESC_CHAR);
  do {
    printMaskedToken(newState!=_WS);
    skipEscapeSequence();
    newState= getCurrentState();
    switch(newState)
      {
      case _EOL:
        insertLineNumber();
        exitState=true;
        break;
      case _WS:
        processWsState();
        --lineIndex;
        break;
      case _EOF:
        eof = true;
        break;
      default:
        exitState=newState!=ESC_CHAR;
        break;
      }
  } while ((!exitState) && (!eof));

  closeTag(ESC_CHAR);
  return eof;
}

void  CodeGenerator::skipEscapeSequence(){
  if (lineIndex<line.length()){
    char c=line[lineIndex];
    int charsToSkip=1;
    // Escape Sequenz /ooo Oktal, /x000 hex, /u00xx Java unicode
    if (isdigit(c) ){
      // \0 abfangen
      while ( isdigit(line[lineIndex+charsToSkip]) && charsToSkip<4) {
        ++charsToSkip;
      }
    } else if (tolower(c)=='x'){
      charsToSkip=langInfo.isJava() ? 4 : 3;
    } else if (tolower(c)=='u'){
      charsToSkip=5;
    }
    while (charsToSkip-- && lineIndex++<line.length()){
       *out <<maskCharacter(line[lineIndex-1]);
    }
  }
}


void CodeGenerator::processWsState()
{
  if (!maskWs) {
    wsBuffer += token;
    token.clear();
    return;
  }
  flushWs();
  int cntWs=0;
  lineIndex--;

   while (isspace(line[lineIndex])  ) {
    ++cntWs;
    ++lineIndex;
  }

  if (cntWs>1) {
    unsigned int styleID=getStyleID(currentState, currentKeywordClass);
    if (excludeWs && styleID!=_UNKNOWN) {
        *out << styleTagClose[styleID];
    }
    *out << maskWsBegin;
    for (int i=0; i<cntWs; i++){
        *out << spacer;
    }
    *out << maskWsEnd;
    if (excludeWs && styleID!=_UNKNOWN){
        *out << styleTagOpen[styleID];
    }
  } else {
    *out << token;
  }
  token.clear();
}

void CodeGenerator::flushWs(){
   *out<<wsBuffer;
   wsBuffer.clear();
}

bool CodeGenerator::isFirstNonWsChar() {
  unsigned int i=lineIndex-1;
  while (i--){
    if (!isspace(line[i])){
      return false;
    }
  }
  return true;
}

string CodeGenerator::getNewLine(){
  return newLineTag;
}

void CodeGenerator::insertLineNumber(bool insertNewLine) {
    if (insertNewLine){
    wsBuffer += getNewLine();
  }
  if (showLineNumbers) {
    ostringstream os;
    ostringstream numberPrefix;
    if (lineNumberFillZeroes) {
      os.fill('0');
    }
    os <<setw(LINE_NUMBER_WIDTH) << right << lineNumber;

    numberPrefix << styleTagOpen[LINENUMBER]
         << maskString(os.str()) << spacer
         << styleTagClose[LINENUMBER];

    wsBuffer += numberPrefix.str();
  }
}

unsigned int CodeGenerator::getLineIndex(){
  return lineIndex;
}

bool CodeGenerator::printExternalStyle(const string &outFile)
{
    if (!includeStyleDef && langInfo.getSyntaxHighlight()) {
        ofstream cssOutFile(outFile.c_str());
        if (cssOutFile) {
            cssOutFile << styleCommentOpen
                    <<" Style definition file generated by highlight "
                    << HIGHLIGHT_VERSION << ", " << HIGHLIGHT_URL
                    << " " << styleCommentClose << "\n";
            cssOutFile << "\n"<<styleCommentOpen<<" Highlighting theme definition: "
                    << styleCommentClose << "\n\n"
                    << getStyleDefinition()
                    << "\n";
            cssOutFile << readUserStyleDef();
            cssOutFile.close();
        } else {
            return false;
        }
    }
    return true;
}

string CodeGenerator::readUserStyleDef(){
    ostringstream ostr;
    if (!styleInputPath.empty()){
        ifstream userStyleDef(styleInputPath.c_str());
        if (userStyleDef) {
            ostr << "\n"<<styleCommentOpen<<" Content of "<<styleInputPath<<": "<<styleCommentClose<<"\n";
            string line;
            while (getline(userStyleDef, line)){
                ostr << line << "\n";
            }
            userStyleDef.close();
        } else {
            ostr << styleCommentOpen<<" ERROR: Could not include "
                    << styleInputPath
                    << "."<<styleCommentClose<<"\n";
        }
    }
    return ostr.str();
}

bool CodeGenerator::checkSpecialCmd(){
    bool insertNL = (lineIndex-token.length());
    cerr << "token: "<<token<< " index"<< lineIndex << " "<<line [ lineIndex ]<<endl;

    if (line [ lineIndex ]=='!'){
      // find cmd
      size_t cmdPos1 = line.find('@', lineIndex+1);

      cerr << "cmdPos"<<cmdPos1<<endl;
      if(cmdPos1==string::npos) return false;
      size_t cmdPos2=cmdPos1+1;
      while (cmdPos2 < line.length() && StringTools::isAlpha(line[cmdPos2])) cmdPos2++;
      cerr << "cmdPos2"<<cmdPos2<<endl;
      cerr << line.substr(cmdPos1, cmdPos2)<<endl;

      // hide comment line
      token.clear();
      lineIndex=line.length();
      getInputChar(); lineNumber--;
      if (insertNL) { lineNumber++;insertLineNumber();};
      // end hide

      return true;
    }

    return false;
}

}
/***************************************************************************
                          codeparser.h  -  description
                             -------------------
    begin                : Die Jul 9 2002
    copyright            : (C) 2002 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef CODEPARSER_H
#define CODEPARSER_H

#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>
#include <cctype>

#include "languagedefinition.h"
#include "documentstyle.h"
#include "ASFormatter.h"
#include "preformatter.h"
#include "enums.h"


#define NUMBER_BUILTIN_STYLES 10
#define LINE_NUMBER_WIDTH     5
#define MAX_LINE__WIDTH       80

#define OUTPUT_FLAG_LN    1
#define OUTPUT_FLAG_LNZ   2
#define OUTPUT_FLAG_FRAG  4

/** The highlight namespace contains all classes and data structures
    needed for parsing input data.
*/
namespace highlight {

/** \brief Base class for parsing. Works similar to a Turing machine.

    The virtual class provides source code parsing functioality, based on
    information stored in language definitions.<br>
    Deriving classes have to define the output format.<br>
    Codegenerator is a singleton class.

* @author Andre Simon
*/

class CodeGenerator
  {

  public:

    virtual ~CodeGenerator();

    /**
      Get appropriate Codegenerator instance
      \param type Output file type (HTML, XHTML, RTF, LATEX, TEX, XSLFO, ANSI)
      \param styleInfoPath Path to formatting style information
      \param styleInPath Path to style definition input file (to be included in styleOutPath)
      \param styleOutPath Path to style definition output file (CSS path for HTML output)
      \param encoding Output file encoding name
      \param includeStyle Switch to include style information in output file (only XHTML, HTML)
      \param attachAnchors Switch to attach anchors to line numbers (only XHTML, HTML)
      \param replaceQuotes Switch to replace quotes by \dq{} (only LATEX)
      \param fopCompatible Switch to generate FO for Apache FOP  (only XSLFO)
      \param omitEncoding Switch to omit encoding info in output document
      \param ln Set true if line numbers should be printed
      \param lnz Set true if leading space of line numbers should be filled with 0's
      \param fragment Set true if document header and footer should be omitted
      \param numSpaces Number of spaces which replace a tab
      \param lineWrappingMode Line wrapping mode
    */
    static CodeGenerator* getInstance(OutputType type,
                                      const string& styleInfoPath,
                                      const string& styleInPath,
                                      const string& styleOutPath,
                                      const string& encoding,
                                      bool includeStyle,
                                      bool attachAnchors,
                                      bool replaceQuotes,
                                      bool fopCompatible,
                                      int numSpaces,
                                      WrapMode lineWrappingMode,
                                      bool ln,
                                      bool lnz,
                                      bool fragment,
                                      bool omitEncoding );

    /** Deletes the singleton CodeGenerator instance.
        Call this method if getInstance was already called, or if you want to
        free the momory after usage.*/
    static void deleteInstance();

    /**
     Generates output
     \param inFileName Path of input file (if empty use stdin)
     \param outFileName Path of input file (if empty use stdout)

     \return ParseError
    */
    ParseError printOutput(const string &inFileName, const string &outFileName);

    /** \return True if document style was found */
    bool styleFound();

    /** \return True if reformatting of current input is disabled */
    bool formattingDisabled();

    /** \return True if reformatting of current input is possible */
    bool formattingIsPossible();

    /** \param langDefPath Absolute path to language definition
        \return  Failure: LOAD_FAILED; Reload necessary: LOAD_NEW,
                 no reload necessary:  LOAD_NONE */
    LoadResult initLanguage(const string& langDefPath);

    /** \return Language definition*/
    LanguageDefinition &getLanguage();

    /** tell parser to output line numbers
       \param  flag true if line numbers should be printed
    */
    void setPrintLineNumbers(bool flag);

    /** \return line number flag */
    bool getPrintLineNumbers();


    /** tell parser to output line numbers filled with zeroes
        \param  flag true if zeroes should be printed
    */
    void setPrintZeroes(bool flag);

    /** \return print zeroes flag */
    bool getPrintZeroes();

    /** tell parser to omit document header and footer
       \param  flag true if output should be fragmented
    */
    void setFragmentCode(bool flag);

    /** \return fragment flag */
    bool getFragmentCode();

    /** tell parser the style name
       \param s path to style definition
    */
    void setStyleName(const string& s);

    /** \return style path */
    const string& getStyleName();

    /** tell parser the wrapping mode
       \param lineWrappingStyle wrapping style
       \param lineLength max line length
       \param numberSpaces number of spaces which replace a tab
    */
    void setPreformatting(WrapMode lineWrappingStyle, unsigned int lineLength,int numberSpaces);

    /** \return wrapping style */
    WrapMode getLineWrapping();

    /** tell parser the include style definition in output
        \param flag true if style should be included
     */
    void setIncludeStyle(bool flag);

   /** Print style definitions to external file
     \param outFile Path of external style definition
     */
    bool printExternalStyle(const string &outFile);

   /** Print index file with all input file names
      \param fileList List of output file names
      \param outPath Output path
    */
    virtual bool printIndexFile(const vector<string> & fileList,
                                const string &outPath);

   /** initialize source code indentation
       \param indentSchemePath Path of indentation scheme
       \return true id successfull
    */
    bool initIndentationScheme(const string&indentSchemePath);

    /** Set style input path
       \param s path to style input file
     */
    void setStyleInputPath(const string& path);

    /** Set style output path
      \param s path to style output file
    */
    void setStyleOutputPath(const string& path);

/** Set output type
    \param s output type
 */
    void setType(OutputType t);

    /**
       \return style input file path
    */
    const string&  getStyleInputPath();

    /**
       \return style output file path
     */
    const string&  getStyleOutputPath();

protected:

    CodeGenerator();

    //! CodeGenerator Constructor
    /**
        \param colourTheme Name of coloring style being used
    */
    CodeGenerator(const string &colourTheme);

    /** \param c Character to be masked
        \return Escape sequence of output format */
    virtual string maskCharacter(unsigned char c) = 0;

    /** \param s string
       \return Copy of s with all escaped characters */
    string maskString(const string &s ) ;

    /** \param s Symbol string
        \param searchPos Position where search starts
        \return Found state (integer value)  */
    State getState(const string &s, unsigned int searchPos);

    /** \return Next identifier in current line of code */
    string getIdentifier();

    /** \return Next number in current line of code */
    string getNumber();

    /** Insert line number at the beginning of current output line */
    virtual void insertLineNumber(bool insertNewLine=true);

    /** Prints document footer*/
    virtual string getFooter() = 0;

    /** Prints document body*/
    virtual void printBody() = 0;

    /** prints document header
       \param  title Title of the document
    */
    virtual string getHeader(const string &title) = 0;

    /** Get current line number
      \return line number  */
    unsigned int getLineNumber();


    /** Tag Delimiters for every colour style*/
    vector <string> styleTagOpen, styleTagClose;

    /** Description of document colour style*/
    DocumentStyle docStyle;

    /** Language definition*/
    LanguageDefinition langInfo;

    /** Tag for inserting line feeds*/
    string newLineTag;

    /** String that represents a white space in output */
    string spacer;

    /** file input*/
    istream *in;

    /** file output*/
    ostream *out;

    /** Tags which enclose white space indentation blocks */
    string maskWsBegin, maskWsEnd;

    /** Style comment delimiters */
    string styleCommentOpen, styleCommentClose;

    /** Test if maskWsBegin and maskWsEnd should be applied */
    bool maskWs;

    /** Test if whitespace sould always be separated from enclosing tokens */
    bool excludeWs;

    /** Test if header and footer should be omitted */
    bool fragmentOutput;

    /** Test if line numbers should be printed */
    bool showLineNumbers;

    /** Test if leading spyce of line number should be filled with zeroes*/
    bool lineNumberFillZeroes;

    /** Current line of input file*/
    string line;

    /** Current line number */
    unsigned int lineNumber;

    // Zeigt den aktuellen Zustand an
    // wird nicht in getCurrentState gesetzt, da nur ZustÃ¤nde interessant
    // sind, die als Index auf die styleCloseTags und styleOpenTags verwendet
    // werden können
    /** Current state*/
    State currentState;

    /** keyword class id, used to apply the corresponding keyword style*/
    unsigned int currentKeywordClass;

    /** Processes origin state */
    void processRootState();

    /** return line break sequence */
    virtual string getNewLine();

    /**
       \param s current state
       \return Index of style tag corresponding to the states
    */
    unsigned int getStyleID(State s, unsigned int kwClassID = 0);

    /** \return line index */
    unsigned int  getLineIndex();

    /** print all remaining white space*/
    void flushWs();

    /**
    \return Content of user defined input style
    */
    string readUserStyleDef();

    /**
     \return Style definition of the chosen output format
    */
    virtual string  getStyleDefinition() {return "";};

    /** contains white space, which will be printed after a closing tag */
    string wsBuffer;

    /**
       Flag to test if style definition should be included in output document
     */
    bool includeStyleDef;

private:

    CodeGenerator(const CodeGenerator&){}
    CodeGenerator& operator=(CodeGenerator&){ return *this;}

    static CodeGenerator* generator;

    /** return matching open and close tags of the given state */
    virtual string getMatchingOpenTag(unsigned int) = 0;
    virtual string getMatchingCloseTag(unsigned int) = 0;

   /** open a new tag, set current state to s*/
    void openTag(State s);

    /** close opened tag, clear current state */
    void closeTag(State s);

    void closeTag(unsigned int styleID);

    void openTag(unsigned int styleID);

    // path to style definition file
    string  stylePath;

    // contains current position in line
    unsigned int lineIndex;

    /**last character of the last line*/
    unsigned char terminatingChar;

    /** Class for reformatting */
    astyle::ASFormatter *formatter;

    /** Class for line wrapping and tab replacement*/
    PreFormatter *preFormatter;

    /** Flag to test if formatting is enabled with current input document*/
    bool formattingEnabled;


    /** Flag to test if formatting is possible with current input document*/
    bool formattingPossible;

    /** contains the current token*/
    string token;

    string styleInputPath, styleOutputPath;

    /** Resets parser to origin state, call this after every file conversion */
    void reset();

    /** read new line from in stream */
    bool readNewLine(string &newLine);

    /** return next character from in stream */
    unsigned char getInputChar();

    OutputType outputType;

   /** return new state */
    State getCurrentState ( bool lastStateWasNumber=false);

    /** Methods that represent a parsing state */
    bool processKeywordState(State myState) ;
    bool processNumberState() ;
    bool processMultiLineCommentState();
    bool processSingleLineCommentState();
    bool processStringState(State oldState);
    bool processEscapeCharState();
    bool processDirectiveState();
    bool processTagState();
    bool processSymbolState();
    void processWsState();

    /** gibt true zurck, falls c ein erlaubter Character innerhalb von Keyword
        oder Typbezeichner ist */
    bool isAllowedChar(char c) ;

    /** returns true if curret token is the first in line and no whitespace */
    bool isFirstNonWsChar() ;

    /** print escaped token and clears it */
    void printMaskedToken(bool flushWhiteSpace=true);

    /** print escape sequence */
    void skipEscapeSequence();

    void closeKWTag(unsigned int styleID);
    void openKWTag(unsigned int styleID);

    /** look for special commands in comments, take action in derived class
        \return true if command was found
    */
    bool checkSpecialCmd();

  };
}

#endif

/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * compiler_defines.h   (1 January 1999)
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C++, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 */





/*
 * comment out the line below if your compiler does NOT understand NAMESPACES
 */
#define USES_NAMESPACE


#if defined(__GNUC__) && __GNUC__ < 3
// for G++ implementation of string.compare:
#define COMPARE(place, length, str)       compare((str), (place), (length))
#else
// for standard implementation of string.compare:
#define COMPARE(place, length, str)       compare((place), (length), (str))
#endif


// Fix by John A. McNamara
// Get rid of annoying MSVC warnings on debug builds about lengths of
// identifiers in template instantiations.
#ifdef _MSC_VER
#pragma warning( disable:4786 )
#endif

/***************************************************************************
                          configurationreader.cpp  -  description
                             -------------------
    begin                : Son Nov 10 2002
    copyright            : (C) 2002 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "configurationreader.h"

using namespace std;

ConfigurationReader::ConfigurationReader(const string & configuration_path)
{
  ifstream in (configuration_path.c_str());
  fileFound=in;
  if (fileFound) {
    string line;
    line.reserve(500);
    unsigned int  lineBegin;
    size_t delimPos;
    string paramName, paramValue;
    while (getline(in, line)) {
      lineBegin=line.find_first_not_of("\t ");
      if ((line.size()>2) && (lineBegin!=string::npos)
            && (line.at(lineBegin)!='#')) {  //comment?
          if (line[lineBegin]=='$') {       // neuer Parametername?
             delimPos=line.find("=",lineBegin)-1;
             if (delimPos!=string::npos) {
                 paramName=StringTools::trimRight(
                           StringTools::lowerCase(line.substr(lineBegin+1, delimPos)));
                 parameterNames.push_back(paramName);
                 paramValue=line.substr(delimPos+2, line.length());
              }
            } else  { // line belongs to last parameter
                paramValue=line;
            }
            if (parameterMap[paramName].empty())  {
               parameterMap[paramName] = paramValue;
            } else {
                parameterMap[paramName]+= (" "+paramValue);
            }
         }  //if ((lineBegin!=string::npos) && (line.at(lineBegin)!='#'))
      } //while
    in.close();
  } //if (in)
}

ConfigurationReader::~ConfigurationReader()
{
}

bool ConfigurationReader::found()
{
  return fileFound;
}

string &ConfigurationReader::getParameter(const string & paramName)
{
  return parameterMap[paramName] ;
}

const char* ConfigurationReader::getCParameter(const string & paramName)
{
  return parameterMap[paramName].c_str() ;
}

vector<string> &ConfigurationReader::getParameterNames()
{
  return parameterNames;
}
/***************************************************************************
                          configurationreader.h  -  description
                             -------------------
    begin                : Son Nov 10 2002
    copyright            : (C) 2002 by Andrï¿½Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef CONFIGURATIONREADER_H
#define CONFIGURATIONREADER_H

#include <string>
#include <sstream>
#include <map>
#include <iostream>
#include <fstream>
#include <vector>

#include "stringtools.h"

using namespace std;

/** Maps parameter keys to values*/
typedef map<string, string> ParameterMap;


/** \brief Class to handle ASCII config files

  Configuration file format:<br>
  $ParamName=ParamValue<br>
  ParamValue may be splittet over multiple lines<br>
  ParamName is not case sensitive<br>
  Comments start with # as the first character of a line

 **/

class ConfigurationReader
  {
  public:
    /** Constructor
        \param configuration_path Path to configuration file
    */
    ConfigurationReader(const string & configuration_path);
    ~ConfigurationReader();

    /** \param paramName Name of parameter
        \return Value of parameter */
    string &getParameter(const string & paramName);

    /** \param paramName Name of parameter
        \return Value of parameter */
    const char* getCParameter(const string & paramName);

    /** \return True if config file exists */
    bool found();

    /** \return List of parameter names */
    vector<string> &getParameterNames();

  private:
    ParameterMap parameterMap;
    bool fileFound;
    vector<string> parameterNames;
  };

#endif
/***************************************************************************
                          dataDir.cpp  -  description
                             -------------------
    begin                : Sam Mï¿½ 1 2003
    copyright            : (C) 2003 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "datadir.h"

using namespace std;

bool DataDir::searchDataDir(const string &userDefinedDir){
#ifndef _WIN32

  bool found = false;
  //falls kein Datenverzeichnis angegeben, startIndex auf 1 setzen
  int searchStartIndex=(userDefinedDir.empty());

  string possibleDirs[] ={ userDefinedDir,
                         #ifdef HL_DATA_DIR
                           HL_DATA_DIR,
                         #endif
                           "/usr/share/highlight/"
                         };

  for (int i=searchStartIndex;i<
       #ifdef HL_DATA_DIR
       3
       #else
       2
       #endif
       ;i++)
    {
      if (fileExists(possibleDirs[i]))
        {
          dataDir=possibleDirs[i];
          found = true;
        }
      if (found) {
       break;
      }
      else {
        if (!searchStartIndex)
         cerr << "highlight: directory "
              << userDefinedDir
              << " specified by data-dir option not found.\n"
              << "           Searching another standard directory.\n";

      }
    }
  return found;
#else
  dataDir=userDefinedDir;
  return true;
#endif

}

DataDir::DataDir()
{
}

void DataDir::setAdditionalDataDir(const string& dir){
  additionalDataDir=dir;
}

const string &DataDir::getDir()
{
  return dataDir;
}

const string DataDir::getLangDefDir()
{
  return dataDir+"langDefs"+Platform::pathSeparator;
}

const string DataDir::getThemeDir()
{
  return dataDir+"themes"+Platform::pathSeparator;
}

const string DataDir::getIndentSchemesDir()
{
  return dataDir+"indentSchemes"+Platform::pathSeparator;
}


const string DataDir::getAdditionalLangDefDir()
{
  return additionalDataDir+"langDefs"+Platform::pathSeparator;
}

const string DataDir::getAdditionalThemeDir()
{
  return additionalDataDir+"themes"+Platform::pathSeparator;
}
const string DataDir::getAdditionalIndentSchemesDir()
{
  return additionalDataDir+"indentSchemes"+Platform::pathSeparator;
}


const string DataDir::getHelpMsgDir()
{
  return dataDir+"helpmsg"+Platform::pathSeparator;
}

const string DataDir::searchForLangDef(const string & langDef){
    if (!additionalDataDir.empty()){
       string path=getAdditionalLangDefDir()+langDef;
       if (fileExists(path)){
          return path;
       }
    }
    return getLangDefDir()+langDef;
}

const string DataDir::searchForTheme(const string & theme){
    if (!additionalDataDir.empty()){
       string path=getAdditionalThemeDir()+theme;
       if (fileExists(path)){
          return path;
       }
    }
    return getThemeDir()+theme;
}

const string DataDir::searchForIndentScheme(const string & scheme){
    if (!additionalDataDir.empty()){
       string path=getAdditionalIndentSchemesDir()+scheme;
       if (fileExists(path)){
          return path;
       }
    }
    return getIndentSchemesDir()+scheme;
}


bool DataDir::fileExists(const string&f){
  ifstream file(f.c_str());
  bool exists=!file.fail();
  file.close();
  return exists;
}
/***************************************************************************
                          datadir.h  -  description
                             -------------------
    begin                : Sam Mï¿½ 1 2003
    copyright            : (C) 2003 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef DATADIR_H
#define DATADIR_H

#include <string>
#include <fstream>
#include <iostream>
//#include "stringtools.h"
#include "platform_fs.h"

using namespace std;

 /** \brief Manages access to installation directories.

     Apart from the standard installation directory, one can define additional
     search paths.
  **/

class DataDir
  {
     string dataDir;
     string additionalDataDir;
     bool fileExists(const string&f);

  public:

     DataDir();

     /** search for a valid installation directory
         \param  userDefinedDir Directory defined by user
         \return True if directory was found */
     bool searchDataDir(const string &userDefinedDir);

     /** add another installation directory, which is added to search path
         \param  dir Directory defined by user */
     void setAdditionalDataDir(const string& dir);

    /** \return Data installation directory */
     const string & getDir() ;

    /** \return Location of languafe definitions */
     const string getLangDefDir() ;

    /** \return Location of themes */
     const string getThemeDir() ;

    /** \return Location of indentation schemes */
    const string getIndentSchemesDir();

    /** \return User defined location of indentation schemes */
    const string getAdditionalIndentSchemesDir();

     /** \return User defined location of languafe definitions */
     const string getAdditionalLangDefDir() ;

    /** \return User defined location of themes */
     const string getAdditionalThemeDir() ;

    /** \return Location of help files */
     const string getHelpMsgDir() ;

     /** \param langDef Name of language definition
         \return Absolute path of definiton found in a data directory */
     const string searchForLangDef(const string & langDef);

     /** \param theme Name of colour theme file
         \return Absolute path of theme found in a data directory */
     const string searchForTheme(const string & theme);

     /** \param scheme Name of indent scheme file
         \return Absolute path of theme found in a data directory */
     const string searchForIndentScheme(const string & scheme);
  };

#endif
/***************************************************************************
                          documentstyle.cpp  -  description
                             -------------------
    begin                : Son Nov 10 2002
    copyright            : (C) 2002 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "documentstyle.h"

namespace highlight {

DocumentStyle::DocumentStyle(const string &styleDefinitionFile)
{
  fileFound=load(styleDefinitionFile);
}
DocumentStyle::DocumentStyle():fileFound(false)
{}

bool DocumentStyle::load(const string &styleDefinitionPath)
{
  ConfigurationReader styleConfig(styleDefinitionPath);
  if (styleConfig.found()){
    fontsize = styleConfig.getParameter("fontsize");
    bgColour.setRGBValues(styleConfig.getParameter("bgcolour"));
    defaultElem.set(styleConfig.getParameter("defaultcolour"));
    comment.set(styleConfig.getParameter("comment"));
    directive.set(styleConfig.getParameter("directive"));
    str.set(styleConfig.getParameter("string"));
    escapeChar.set(styleConfig.getParameter("escapechar"));
    number.set(styleConfig.getParameter("number"));
    dstr.set(styleConfig.getParameter("string_directive"));
    line.set(styleConfig.getParameter("line"));


    string tmpstr;
  // TODO: Remove this check as soon as all themes have a brackets attribute
    tmpstr=styleConfig.getParameter("symbol");
    if (tmpstr.empty()) {
      tmpstr=styleConfig.getParameter("defaultcolour");
    }
    symbol.set(tmpstr);

// TODO: Remove this check as soon as all themes have a sl-comment attribute
    tmpstr=styleConfig.getParameter("sl-comment");
    if (tmpstr.empty()) {
      tmpstr=styleConfig.getParameter("comment");
    }
    slcomment.set(tmpstr);

    string paramVal;
    vector<string> paramNames=styleConfig.getParameterNames();

    //collect keyword classes, save corresponding style definition
    for (unsigned int i=0;i<paramNames.size();i++){
      paramVal=paramNames[i];
      if (paramVal.find("kw_class") != string::npos){
         keywordStyles.insert(make_pair(StringTools::getParantheseVal(paramVal),
                              new ElementStyle(styleConfig.getParameter(paramVal))));
      }
    }

    fileFound = true;
  }
  else {
    fileFound = false;
  }
  return fileFound;
}

DocumentStyle::~DocumentStyle()
{
  for(KSIterator iter = keywordStyles.begin(); iter != keywordStyles.end(); iter++){
    delete (*iter).second;  //remove ElementStyle*
  }
}

string& DocumentStyle::getFontSize()
  {
    return fontsize;
  }
StyleColour& DocumentStyle::getBgColour()
  {
    return bgColour;
  }
ElementStyle& DocumentStyle::getDefaultStyle()
  {
    return defaultElem;
  }
ElementStyle& DocumentStyle::getCommentStyle()
  {
    return comment;
  }
ElementStyle& DocumentStyle::getSingleLineCommentStyle()
  {
    return slcomment;
  }


ElementStyle& DocumentStyle::getStringStyle()
  {
    return str;
  }
ElementStyle& DocumentStyle::getDirectiveStringStyle()
  {
    return dstr;
  }
ElementStyle& DocumentStyle::getEscapeCharStyle()
  {
    return escapeChar;
  }
ElementStyle& DocumentStyle::getNumberStyle()
  {
    return number;
  }
ElementStyle& DocumentStyle::getDirectiveStyle()
  {
    return directive;
  }
ElementStyle& DocumentStyle::getLineStyle()
  {
    return line;
  }
ElementStyle& DocumentStyle::getSymbolStyle()
  {
    return symbol;
  }
bool DocumentStyle::found () const
  {
    return fileFound;
  }
ElementStyle& DocumentStyle::getKeywordStyle(const string &className){
  if (!keywordStyles.count(className)) return defaultElem;
  return *keywordStyles[className];
}

vector <string> DocumentStyle::getClassNames(){
  vector <string> kwClassNames;
  for(KSIterator iter = keywordStyles.begin(); iter != keywordStyles.end(); iter++){
    kwClassNames.push_back( (*iter).first);
  }
  return kwClassNames;
}

KeywordStyles& DocumentStyle::getKeywordStyles(){
  return keywordStyles;
}

}
/***************************************************************************
                          documentstyle.h  -  description
                             -------------------
    begin                : Son Nov 10 2002
    copyright            : (C) 2002 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef DOCUMENTSTYLE_H
#define DOCUMENTSTYLE_H

#include <string>
#include <iostream>
#include "configurationreader.h"
#include "elementstyle.h"
#include "stylecolour.h"

using namespace std;

namespace highlight {

/** maps keyword class names and the corresponding formatting information*/
typedef map <string, ElementStyle*> KeywordStyles;

/** iterator for keyword styles*/
typedef KeywordStyles::iterator KSIterator;

/** \brief Contains information about document formatting properties.

* @author Andre Simon
*/

class DocumentStyle
  {
  private:
    ElementStyle comment, slcomment, str, dstr,
                 escapeChar, number, directive, line, symbol;
    ElementStyle defaultElem;
    StyleColour  bgColour;

    string fontsize;
    bool fileFound;

    KeywordStyles keywordStyles;

  public:
    /** Constructor
        \param styleDefinitionPath Style definition path */
    DocumentStyle(const string & styleDefinitionPath);
    DocumentStyle();
    ~DocumentStyle();

  /** load sytle definition
        \param styleDefinitionFile Style definition path
        \return True if successfull */
    bool load(const string & styleDefinitionFile);

    /** \return class names defined in the theme file */
    vector <string> getClassNames();

    /** \return keyword styles */
    KeywordStyles& getKeywordStyles();

    /** \return Font size */
    string &getFontSize() ;

    /** \return Background colour*/
    StyleColour& getBgColour();

    /** \return Style of default (unrecognized) strings */
    ElementStyle & getDefaultStyle() ;

    /** \return Comment style*/
    ElementStyle & getCommentStyle() ;

    /** \return Single line comment style*/
    ElementStyle& getSingleLineCommentStyle() ;

    /** \return Keyword style*/
    ElementStyle & getKeywordStyle() ;

    /** \return String style*/
    ElementStyle & getStringStyle() ;

    /** \return Directive line string style*/
    ElementStyle & getDirectiveStringStyle() ;

    /** \return Escape character style*/
    ElementStyle & getEscapeCharStyle() ;

    /** \return Number style*/
    ElementStyle & getNumberStyle() ;

    /** \return Directive style*/
    ElementStyle & getDirectiveStyle() ;

    /** \return Type style*/
    ElementStyle & getTypeStyle() ;

    /** \return Line number style*/
    ElementStyle & getLineStyle() ;

    /** \return Bracket style*/
    ElementStyle & getSymbolStyle() ;

    /**
       \param className Name of keyword class
       \return keyword style of the given className
    */
    ElementStyle & getKeywordStyle(const string &className);

    /** \return True if language definition was found */
    bool found() const ;
  };

}

#endif
/***************************************************************************
                          elementstyle.cpp  -  description
                             -------------------
    begin                : Son Nov 10 2002
    copyright            : (C) 2002 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "elementstyle.h"

namespace highlight {

ElementStyle::ElementStyle(StyleColour col, bool b, bool i, bool u)
    : colour(col) , bold(b), italic(i), underline(u)
{}

ElementStyle:: ElementStyle(const string & elementStyleString)
  : bold(false), italic(false), underline(false)
{
  set(elementStyleString);
}

ElementStyle::ElementStyle()
    : bold(false), italic(false), underline(false)
{}

void ElementStyle::set(const string & elementStyleString){

  istringstream valueStream(elementStyleString.c_str());
  string r, g, b, attr;
  valueStream >> r;
  valueStream >> g;
  valueStream >> b;
  colour.setRedValue(r);
  colour.setGreenValue(g);
  colour.setBlueValue(b);
  while ( valueStream >> attr)
    {
      if (attr=="italic")
        {
          italic = true;
        }
      else if (attr=="bold")
        {
          bold = true;
        }
      else if (attr=="underline")
        {
          underline = true;
        }
    }
}

ElementStyle::~ElementStyle()
{}

bool ElementStyle::isItalic() const
{
  return italic;
}
bool ElementStyle::isBold() const
{
  return bold;
}
bool ElementStyle::isUnderline() const
{
  return underline;
}
StyleColour ElementStyle::getColour() const
{
  return colour;
}

}
/***************************************************************************
                          elementstyle.h  -  description
                             -------------------
    begin                : Son Nov 10 2002
    copyright            : (C) 2002 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef ELEMENTSTYLE_H
#define ELEMENTSTYLE_H

#include <sstream>

#include "stylecolour.h"

using namespace std;

namespace highlight {

/** \brief The class stores the basic text formatting properties.

* @author Andre Simon
*/

class ElementStyle {
  public:

    /** Constructor
        \param col Style colour
        \param b Bold flag
        \param i Italic flag
        \param u Underline flag */
    ElementStyle(StyleColour col, bool b, bool i, bool u);

   /** Constuctor
        \param elementStyleString String with fotmatting information */
    ElementStyle(const string & elementStyleString);

    ElementStyle();

    ~ElementStyle();

   /** initialize object
       \param elementStyleString String which contains formatting attributes
   */
   void set(const string & elementStyleString);

   /** \return True if italic */
    bool isItalic() const;

   /** \return True if bold */
   bool isBold() const;

   /** \return True if underline */
   bool isUnderline() const;

   /** \return Element colour */
   StyleColour getColour() const;

 private:
    StyleColour colour;
    bool bold, italic, underline;
  };

}

#endif
//
// C++ Interface: enums
//
// Description:
//
//
// Author: Andre Simon <andre.simon1@gmx.de>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef ENUMS_H
#define ENUMS_H

namespace highlight {

/** states which may occour during input file parsing*/
enum State {
  STANDARD=0,
  STRING,
  NUMBER,
  SL_COMMENT,
  ML_COMMENT_BEGIN,
  ESC_CHAR,
  DIRECTIVE_LINE,
  DIRECTIVE_STRING,
  LINENUMBER,
  SYMBOL,

  // Konstanten ab hier duefen nicht mehr als Array-Indizes benutzt werden!!
  KEYWORD,
  ML_COMMENT_END,
  DIRECTIVE_LINE_END,
  TAG_BEGIN,
  TAG_END,
  KEYWORD_BEGIN,
  KEYWORD_END,

  _UNKNOWN=100,
  _EOL,
  _EOF,
  _WS
} ;

/** Parser return values*/
enum ParseError{
   PARSE_OK,
   BAD_INPUT=1,
   BAD_OUTPUT=2,
   BAD_STYLE=4
};

/** line wrapping modes*/
enum WrapMode {
  WRAP_DISABLED,
  WRAP_SIMPLE,
  WRAP_DEFAULT
};

/** language definition loading results*/
enum LoadResult{
  LOAD_FAILED,
  LOAD_NEW,
  LOAD_NONE
};

/** output formats */
enum OutputType {
  HTML,
  XHTML,
  TEX,
  LATEX,
  RTF,
  XSLFO,
  XML,
  ANSI
};

}

#endif
/* Getopt for GNU.
   NOTE: getopt is now part of the C library, so if you don't know what
   "Keep this file name-space clean" means, talk to roland@gnu.ai.mit.edu
   before changing it!

   Copyright (C) 1987, 88, 89, 90, 91, 92, 1993
   	Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifndef __STDC__
#  ifndef const
#    define const
#  endif
#endif

/* This tells Alpha OSF/1 not to define a getopt prototype in <stdio.h>.  */
#ifndef _NO_PROTO
#define _NO_PROTO
#endif

#include <cstdio>
#include <cstring>
//#include "tailor.h"

/* Comment out all this code if we are using the GNU C Library, and are not
   actually compiling the library itself.  This code is part of the GNU C
   Library, but also included in many other GNU distributions.  Compiling
   and linking in this code is a waste when using the GNU C library
   (especially if it is a shared library).  Rather than having every GNU
   program understand `configure --with-gnu-libc' and omit the object files,
   it is simpler to just do this in the source for each such file.  */

#if defined (_LIBC) || !defined (__GNU_LIBRARY__)


/* This needs to come after some library #include
   to get __GNU_LIBRARY__ defined.  */
#ifdef	__GNU_LIBRARY__
/* Don't include stdlib.h for non-GNU C libraries because some of them
   contain conflicting prototypes for getopt.  */
#include <stdlib.h>
#endif	/* GNU C library.  */

/* If GETOPT_COMPAT is defined, `+' as well as `--' can introduce a
   long-named option.  Because this is not POSIX.2 compliant, it is
   being phased out.  */
/* #define GETOPT_COMPAT */

/* This version of `getopt' appears to the caller like standard Unix `getopt'
   but it behaves differently for the user, since it allows the user
   to intersperse the options with the other arguments.

   As `getopt' works, it permutes the elements of ARGV so that,
   when it is done, all the options precede everything else.  Thus
   all application programs are extended to handle flexible argument order.

   Setting the environment variable POSIXLY_CORRECT disables permutation.
   Then the behavior is completely standard.

   GNU application programs can use a third alternative mode in which
   they can distinguish the relative order of options and other arguments.  */

#include "getopt.h"

/* For communication from `getopt' to the caller.
   When `getopt' finds an option that takes an argument,
   the argument value is returned here.
   Also, when `ordering' is RETURN_IN_ORDER,
   each non-option ARGV-element is returned here.  */

char *optarg = 0;

/* Index in ARGV of the next element to be scanned.
   This is used for communication to and from the caller
   and for communication between successive calls to `getopt'.

   On entry to `getopt', zero means this is the first call; initialize.

   When `getopt' returns EOF, this is the index of the first of the
   non-option elements that the caller should itself scan.

   Otherwise, `optind' communicates from one call to the next
   how much of ARGV has been scanned so far.  */

/* XXX 1003.2 says this must be 1 before any call.  */
int optind = 0;

/* The next char to be scanned in the option-element
   in which the last option character we returned was found.
   This allows us to pick up the scan where we left off.

   If this is zero, or a null string, it means resume the scan
   by advancing to the next ARGV-element.  */

static char *nextchar;

/* Callers store zero here to inhibit the error message
   for unrecognized options.  */

int opterr = 1;

/* Set to an option character which was unrecognized.
   This must be initialized on some systems to avoid linking in the
   system's own getopt implementation.  */

#define BAD_OPTION '\0'
int optopt = BAD_OPTION;

/* Describe how to deal with options that follow non-option ARGV-elements.

   If the caller did not specify anything,
   the default is REQUIRE_ORDER if the environment variable
   POSIXLY_CORRECT is defined, PERMUTE otherwise.

   REQUIRE_ORDER means don't recognize them as options;
   stop option processing when the first non-option is seen.
   This is what Unix does.
   This mode of operation is selected by either setting the environment
   variable POSIXLY_CORRECT, or using `+' as the first character
   of the list of option characters.

   PERMUTE is the default.  We permute the contents of ARGV as we scan,
   so that eventually all the non-options are at the end.  This allows options
   to be given in any order, even with programs that were not written to
   expect this.

   RETURN_IN_ORDER is an option available to programs that were written
   to expect options and other ARGV-elements in any order and that care about
   the ordering of the two.  We describe each non-option ARGV-element
   as if it were the argument of an option with character code 1.
   Using `-' as the first character of the list of option characters
   selects this mode of operation.

   The special argument `--' forces an end of option-scanning regardless
   of the value of `ordering'.  In the case of RETURN_IN_ORDER, only
   `--' can cause `getopt' to return EOF with `optind' != ARGC.  */

static enum
{
  REQUIRE_ORDER, PERMUTE, RETURN_IN_ORDER
} ordering;

#ifdef	__GNU_LIBRARY__
/* We want to avoid inclusion of string.h with non-GNU libraries
   because there are many ways it can cause trouble.
   On some systems, it contains special magic macros that don't work
   in GCC.  */
#include <string.h>
#define	my_index	strchr
#define	my_strlen	strlen
#else

/* Avoid depending on library functions or files
   whose names are inconsistent.  */

#if __STDC__ || defined(PROTO)
  #ifndef _WIN32
    // Solaris compilation fix
    extern "C" {
     char *getenv(const char *name);
     int  strncmp(const char *s1, const char *s2, int n);
    }
   // extern char *getenv(const char *name);
   // extern int  strncmp(const char *s1, const char *s2, int n);
  #endif
  extern int  strcmp (const char *s1, const char *s2);
  static int my_strlen(const char *s);
  static char *my_index (const char *str, int chr);
#else
  #ifndef _WIN32
    extern char *getenv ();
  #endif
#endif

static int
my_strlen (const char *str)

{
  int n = 0;
  while (*str++)
    n++;
  return n;
}

static char *
my_index (     const char *str,
     int chr)

{
  while (*str)
    {
      if (*str == chr)
	return (char *) str;
      str++;
    }
  return 0;
}

#endif				/* GNU C library.  */

/* Handle permutation of arguments.  */

/* Describe the part of ARGV that contains non-options that have
   been skipped.  `first_nonopt' is the index in ARGV of the first of them;
   `last_nonopt' is the index after the last of them.  */

static int first_nonopt;
static int last_nonopt;

/* Exchange two adjacent subsequences of ARGV.
   One subsequence is elements [first_nonopt,last_nonopt)
   which contains all the non-options that have been skipped so far.
   The other is elements [last_nonopt,optind), which contains all
   the options processed since those non-options were skipped.

   `first_nonopt' and `last_nonopt' are relocated so that they describe
   the new indices of the non-options in ARGV after they are moved.

   To perform the swap, we first reverse the order of all elements. So
   all options now come before all non options, but they are in the
   wrong order. So we put back the options and non options in original
   order by reversing them again. For example:
       original input:      a b c -x -y
       reverse all:         -y -x c b a
       reverse options:     -x -y c b a
       reverse non options: -x -y a b c
*/

#if __STDC__ || defined(PROTO)
static void exchange (char **argv);
#endif

static void
exchange (char **argv)

{
  char *temp, **first, **last;

  /* Reverse all the elements [first_nonopt, optind) */
  first = &argv[first_nonopt];
  last  = &argv[optind-1];
  while (first < last) {
    temp = *first; *first = *last; *last = temp; first++; last--;
  }
  /* Put back the options in order */
  first = &argv[first_nonopt];
  first_nonopt += (optind - last_nonopt);
  last  = &argv[first_nonopt - 1];
  while (first < last) {
    temp = *first; *first = *last; *last = temp; first++; last--;
  }

  /* Put back the non options in order */
  first = &argv[first_nonopt];
  last_nonopt = optind;
  last  = &argv[last_nonopt-1];
  while (first < last) {
    temp = *first; *first = *last; *last = temp; first++; last--;
  }
}

/* Scan elements of ARGV (whose length is ARGC) for option characters
   given in OPTSTRING.

   If an element of ARGV starts with '-', and is not exactly "-" or "--",
   then it is an option element.  The characters of this element
   (aside from the initial '-') are option characters.  If `getopt'
   is called repeatedly, it returns successively each of the option characters
   from each of the option elements.

   If `getopt' finds another option character, it returns that character,
   updating `optind' and `nextchar' so that the next call to `getopt' can
   resume the scan with the following option character or ARGV-element.

   If there are no more option characters, `getopt' returns `EOF'.
   Then `optind' is the index in ARGV of the first ARGV-element
   that is not an option.  (The ARGV-elements have been permuted
   so that those that are not options now come last.)

   OPTSTRING is a string containing the legitimate option characters.
   If an option character is seen that is not listed in OPTSTRING,
   return BAD_OPTION after printing an error message.  If you set `opterr' to
   zero, the error message is suppressed but we still return BAD_OPTION.

   If a char in OPTSTRING is followed by a colon, that means it wants an arg,
   so the following text in the same ARGV-element, or the text of the following
   ARGV-element, is returned in `optarg'.  Two colons mean an option that
   wants an optional arg; if there is text in the current ARGV-element,
   it is returned in `optarg', otherwise `optarg' is set to zero.

   If OPTSTRING starts with `-' or `+', it requests different methods of
   handling the non-option ARGV-elements.
   See the comments about RETURN_IN_ORDER and REQUIRE_ORDER, above.

   Long-named options begin with `--' instead of `-'.
   Their names may be abbreviated as long as the abbreviation is unique
   or is an exact match for some defined option.  If they have an
   argument, it follows the option name in the same ARGV-element, separated
   from the option name by a `=', or else the in next ARGV-element.
   When `getopt' finds a long-named option, it returns 0 if that option's
   `flag' field is nonzero, the value of the option's `val' field
   if the `flag' field is zero.

   The elements of ARGV aren't really const, because we permute them.
   But we pretend they're const in the prototype to be compatible
   with other systems.

   LONGOPTS is a vector of `struct option' terminated by an
   element containing a name which is zero.

   LONGIND returns the index in LONGOPT of the long-named option found.
   It is only valid when a long-named option has been found by the most
   recent call.

   If LONG_ONLY is nonzero, '-' as well as '--' can introduce
   long-named options.  */

int
_getopt_internal ( int argc,
     char *const *argv,
     const char *optstring,
     const struct option *longopts,
     int *longind,
     int long_only)

{
  int option_index;

  optarg = 0;

  /* Initialize the internal data when the first call is made.
     Start processing options with ARGV-element 1 (since ARGV-element 0
     is the program name); the sequence of previously skipped
     non-option ARGV-elements is empty.  */

  if (optind == 0)
    {
      first_nonopt = last_nonopt = optind = 1;

      nextchar = NULL;

      /* Determine how to handle the ordering of options and nonoptions.  */

      if (optstring[0] == '-')
	{
	  ordering = RETURN_IN_ORDER;
	  ++optstring;
	}
      else if (optstring[0] == '+')
	{
	  ordering = REQUIRE_ORDER;
	  ++optstring;
	}
   #ifndef _WIN32
      else if (getenv ("POSIXLY_CORRECT") != NULL)
	ordering = REQUIRE_ORDER;
   #endif
      else
	ordering = PERMUTE;
    }

  if (nextchar == NULL || *nextchar == '\0')
    {
      if (ordering == PERMUTE)
	{
	  /* If we have just processed some options following some non-options,
	     exchange them so that the options come first.  */

	  if (first_nonopt != last_nonopt && last_nonopt != optind)
	    exchange ((char **) argv);
	  else if (last_nonopt != optind)
	    first_nonopt = optind;

	  /* Now skip any additional non-options
	     and extend the range of non-options previously skipped.  */

	  while (optind < argc
		 && (argv[optind][0] != '-' || argv[optind][1] == '\0')
#ifdef GETOPT_COMPAT
		 && (longopts == NULL
		     || argv[optind][0] != '+' || argv[optind][1] == '\0')
#endif				/* GETOPT_COMPAT */
		 )
	    optind++;
	  last_nonopt = optind;
	}

      /* Special ARGV-element `--' means premature end of options.
	 Skip it like a null option,
	 then exchange with previous non-options as if it were an option,
	 then skip everything else like a non-option.  */

      if (optind != argc && !strcmp (argv[optind], "--"))
	{
	  optind++;

	  if (first_nonopt != last_nonopt && last_nonopt != optind)
	    exchange ((char **) argv);
	  else if (first_nonopt == last_nonopt)
	    first_nonopt = optind;
	  last_nonopt = argc;

	  optind = argc;
	}

      /* If we have done all the ARGV-elements, stop the scan
	 and back over any non-options that we skipped and permuted.  */

      if (optind == argc)
	{
	  /* Set the next-arg-index to point at the non-options
	     that we previously skipped, so the caller will digest them.  */
	  if (first_nonopt != last_nonopt)
	    optind = first_nonopt;
	  return EOF;
	}

      /* If we have come to a non-option and did not permute it,
	 either stop the scan or describe it to the caller and pass it by.  */

      if ((argv[optind][0] != '-' || argv[optind][1] == '\0')
#ifdef GETOPT_COMPAT
	  && (longopts == NULL
	      || argv[optind][0] != '+' || argv[optind][1] == '\0')
#endif				/* GETOPT_COMPAT */
	  )
	{
	  if (ordering == REQUIRE_ORDER)
	    return EOF;
	  optarg = argv[optind++];
	  return 1;
	}

      /* We have found another option-ARGV-element.
	 Start decoding its characters.  */

      nextchar = (argv[optind] + 1
		  + (longopts != NULL && argv[optind][1] == '-'));
    }

  if (longopts != NULL
      && ((argv[optind][0] == '-'
	   && (argv[optind][1] == '-' || long_only))
#ifdef GETOPT_COMPAT
	  || argv[optind][0] == '+'
#endif				/* GETOPT_COMPAT */
	  ))
    {
      const struct option *p;
      char *s = nextchar;
      int exact = 0;
      int ambig = 0;
      const struct option *pfound = NULL;
      int indfound = 0;

      while (*s && *s != '=')
	s++;

      /* Test all options for either exact match or abbreviated matches.  */
      for (p = longopts, option_index = 0; p->name;
	   p++, option_index++)
	if (!strncmp (p->name, nextchar, s - nextchar))
	  {
	    if (s - nextchar == my_strlen (p->name))
	      {
		/* Exact match found.  */
		pfound = p;
		indfound = option_index;
		exact = 1;
		break;
	      }
	    else if (pfound == NULL)
	      {
		/* First nonexact match found.  */
		pfound = p;
		indfound = option_index;
	      }
	    else
	      /* Second nonexact match found.  */
	      ambig = 1;
	  }

      if (ambig && !exact)
	{
	  if (opterr)
	    fprintf (stderr, "%s: option `%s' is ambiguous\n",
		     argv[0], argv[optind]);
	  nextchar += my_strlen (nextchar);
	  optind++;
	  return BAD_OPTION;
	}

      if (pfound != NULL)
	{
	  option_index = indfound;
	  optind++;
	  if (*s)
	    {
	      /* Don't test has_arg with >, because some C compilers don't
		 allow it to be used on enums.  */
	      if (pfound->has_arg)
		optarg = s + 1;
	      else
		{
		  if (opterr)
		    {
		      if (argv[optind - 1][1] == '-')
			/* --option */
			fprintf (stderr,
				 "%s: option `--%s' doesn't allow an argument\n",
				 argv[0], pfound->name);
		      else
			/* +option or -option */
			fprintf (stderr,
			     "%s: option `%c%s' doesn't allow an argument\n",
			     argv[0], argv[optind - 1][0], pfound->name);
		    }
		  nextchar += my_strlen (nextchar);
		  return BAD_OPTION;
		}
	    }
	  else if (pfound->has_arg == 1)
	    {
	      if (optind < argc)
		optarg = argv[optind++];
	      else
		{
		  if (opterr)
		    fprintf (stderr, "%s: option `%s' requires an argument\n",
			     argv[0], argv[optind - 1]);
		  nextchar += my_strlen (nextchar);
		  return optstring[0] == ':' ? ':' : BAD_OPTION;
		}
	    }
	  nextchar += my_strlen (nextchar);
	  if (longind != NULL)
	    *longind = option_index;
	  if (pfound->flag)
	    {
	      *(pfound->flag) = pfound->val;
	      return 0;
	    }
	  return pfound->val;
	}
      /* Can't find it as a long option.  If this is not getopt_long_only,
	 or the option starts with '--' or is not a valid short
	 option, then it's an error.
	 Otherwise interpret it as a short option.  */
      if (!long_only || argv[optind][1] == '-'
#ifdef GETOPT_COMPAT
	  || argv[optind][0] == '+'
#endif				/* GETOPT_COMPAT */
	  || my_index (optstring, *nextchar) == NULL)
	{
	  if (opterr)
	    {
	      if (argv[optind][1] == '-')
		/* --option */
		fprintf (stderr, "%s: unrecognized option `--%s'\n",
			 argv[0], nextchar);
	      else
		/* +option or -option */
		fprintf (stderr, "%s: unrecognized option `%c%s'\n",
			 argv[0], argv[optind][0], nextchar);
	    }
	  nextchar = (char *) "";
	  optind++;
	  return BAD_OPTION;
	}
    }

  /* Look at and handle the next option-character.  */

  {
    char c = *nextchar++;
    char *temp = my_index (optstring, c);

    /* Increment `optind' when we start to process its last character.  */
    if (*nextchar == '\0')
      ++optind;

    if (temp == NULL || c == ':')
      {
	if (opterr)
	  {
#if 0
	    if (c < 040 || c >= 0177)
	      fprintf (stderr, "%s: unrecognized option, character code 0%o\n",
		       argv[0], c);
	    else
	      fprintf (stderr, "%s: unrecognized option `-%c'\n", argv[0], c);
#else
	    /* 1003.2 specifies the format of this message.  */
	    fprintf (stderr, "%s: illegal option -- %c\n", argv[0], c);
#endif
	  }
	optopt = c;
	return BAD_OPTION;
      }
    if (temp[1] == ':')
      {
	if (temp[2] == ':')
	  {
	    /* This is an option that accepts an argument optionally.  */
	    if (*nextchar != '\0')
	      {
		optarg = nextchar;
		optind++;
	      }
	    else
	      optarg = 0;
	    nextchar = NULL;
	  }
	else
	  {
	    /* This is an option that requires an argument.  */
	    if (*nextchar != '\0')
	      {
		optarg = nextchar;
		/* If we end this ARGV-element by taking the rest as an arg,
		   we must advance to the next element now.  */
		optind++;
	      }
	    else if (optind == argc)
	      {
		if (opterr)
		  {
#if 0
		    fprintf (stderr, "%s: option `-%c' requires an argument\n",
			     argv[0], c);
#else
		    /* 1003.2 specifies the format of this message.  */
		    fprintf (stderr, "%s: option requires an argument -- %c\n",
			     argv[0], c);
#endif
		  }
		optopt = c;
		if (optstring[0] == ':')
		  c = ':';
		else
		  c = BAD_OPTION;
	      }
	    else
	      /* We already incremented `optind' once;
		 increment it again when taking next ARGV-elt as argument.  */
	      optarg = argv[optind++];
	    nextchar = NULL;
	  }
      }
    return c;
  }
}

int
getopt (     int argc,
     char *const *argv,
     const char *optstring)

{
  return _getopt_internal (argc, argv, optstring,
			   (const struct option *) 0,
			   (int *) 0,
			   0);
}

int
getopt_long (     int argc,
     char *const *argv,
     const char *options,
     const struct option *long_options,
     int *opt_index)

{
  return _getopt_internal (argc, argv, options, long_options, opt_index, 0);
}

#endif	/* _LIBC or not __GNU_LIBRARY__.  */

#ifdef TEST

/* Compile with -DTEST to make an executable for use in testing
   the above definition of `getopt'.  */

int
main (argc, argv)
     int argc;
     char **argv;
{
  int c;
  int digit_optind = 0;

  while (1)
    {
      int this_option_optind = optind ? optind : 1;

      c = getopt (argc, argv, "abc:d:0123456789");
      if (c == EOF)
	break;

      switch (c)
	{
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  if (digit_optind != 0 && digit_optind != this_option_optind)
	    printf ("digits occur in two different argv-elements.\n");
	  digit_optind = this_option_optind;
	  printf ("option %c\n", c);
	  break;

	case 'a':
	  printf ("option a\n");
	  break;

	case 'b':
	  printf ("option b\n");
	  break;

	case 'c':
	  printf ("option c with value `%s'\n", optarg);
	  break;

	case BAD_OPTION:
	  break;

	default:
	  printf ("?? getopt returned character code 0%o ??\n", c);
	}
    }

  if (optind < argc)
    {
      printf ("non-option ARGV-elements: ");
      while (optind < argc)
	printf ("%s ", argv[optind++]);
      printf ("\n");
    }

  exit (0);
}

#endif /* TEST */

/* Declarations for getopt.
   Copyright (C) 1989-1994, 1996-1999, 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#ifndef _GETOPT_H

#ifndef __need_getopt
# define _GETOPT_H 1
#endif

/* If __GNU_LIBRARY__ is not already defined, either we are being used
   standalone, or this is the first header included in the source file.
   If we are being used with glibc, we need to include <features.h>, but
   that does not exist if we are standalone.  So: if __GNU_LIBRARY__ is
   not defined, include <ctype.h>, which will pull in <features.h> for us
   if it's from glibc.  (Why ctype.h?  It's guaranteed to exist and it
   doesn't flood the namespace with stuff the way some other headers do.)  */
#if !defined __GNU_LIBRARY__
# include <ctype.h>
#endif

#ifdef	__cplusplus
extern "C" {
#endif

/* For communication from `getopt' to the caller.
   When `getopt' finds an option that takes an argument,
   the argument value is returned here.
   Also, when `ordering' is RETURN_IN_ORDER,
   each non-option ARGV-element is returned here.  */

extern char *optarg;

/* Index in ARGV of the next element to be scanned.
   This is used for communication to and from the caller
   and for communication between successive calls to `getopt'.

   On entry to `getopt', zero means this is the first call; initialize.

   When `getopt' returns -1, this is the index of the first of the
   non-option elements that the caller should itself scan.

   Otherwise, `optind' communicates from one call to the next
   how much of ARGV has been scanned so far.  */

extern int optind;

/* Callers store zero here to inhibit the error message `getopt' prints
   for unrecognized options.  */

extern int opterr;

/* Set to an option character which was unrecognized.  */

extern int optopt;

#ifndef __need_getopt
/* Describe the long-named options requested by the application.
   The LONG_OPTIONS argument to getopt_long or getopt_long_only is a vector
   of `struct option' terminated by an element containing a name which is
   zero.

   The field `has_arg' is:
   no_argument		(or 0) if the option does not take an argument,
   required_argument	(or 1) if the option requires an argument,
   optional_argument 	(or 2) if the option takes an optional argument.

   If the field `flag' is not NULL, it points to a variable that is set
   to the value given in the field `val' when the option is found, but
   left unchanged if the option is not found.

   To have a long-named option do something other than set an `int' to
   a compiled-in constant, such as set a value from `optarg', set the
   option's `flag' field to zero and its `val' field to a nonzero
   value (the equivalent single-letter option character, if there is
   one).  For long options that have a zero `flag' field, `getopt'
   returns the contents of the `val' field.  */

struct option
{
# if (defined __STDC__ && __STDC__) || defined __cplusplus
  const char *name;
# else
  char *name;
# endif
  /* has_arg can't be an enum because some compilers complain about
     type mismatches in all the code that assumes it is an int.  */
  int has_arg;
  int *flag;
  int val;
};

/* Names for the values of the `has_arg' field of `struct option'.  */

# define no_argument		0
# define required_argument	1
# define optional_argument	2
#endif	/* need getopt */


/* Get definitions and prototypes for functions to process the
   arguments in ARGV (ARGC of them, minus the program name) for
   options given in OPTS.

   Return the option character from OPTS just read.  Return -1 when
   there are no more options.  For unrecognized options, or options
   missing arguments, `optopt' is set to the option letter, and '?' is
   returned.

   The OPTS string is a list of characters which are recognized option
   letters, optionally followed by colons, specifying that that letter
   takes an argument, to be placed in `optarg'.

   If a letter in OPTS is followed by two colons, its argument is
   optional.  This behavior is specific to the GNU `getopt'.

   The argument `--' causes premature termination of argument
   scanning, explicitly telling `getopt' that there are no more
   options.

   If OPTS begins with `--', then non-option arguments are treated as
   arguments to the option '\0'.  This behavior is specific to the GNU
   `getopt'.  */

#if (defined __STDC__ && __STDC__) || defined __cplusplus
# ifdef __GNU_LIBRARY__
/* Many other libraries have conflicting prototypes for getopt, with
   differences in the consts, in stdlib.h.  To avoid compilation
   errors, only prototype getopt for the GNU C library.  */
extern int getopt (int ___argc, char *const *___argv, const char *__shortopts);
# else /* not __GNU_LIBRARY__ */
// Solaris compilation fix
//extern int getopt ();
# endif /* __GNU_LIBRARY__ */

# ifndef __need_getopt
extern int getopt_long (int ___argc, char *const *___argv,
			const char *__shortopts,
		        const struct option *__longopts, int *__longind);
extern int getopt_long_only (int ___argc, char *const *___argv,
			     const char *__shortopts,
		             const struct option *__longopts, int *__longind);

/* Internal only.  Users should not call this directly.  */
extern int _getopt_internal (int ___argc, char *const *___argv,
			     const char *__shortopts,
		             const struct option *__longopts, int *__longind,
			     int __long_only);
# endif
#else /* not __STDC__ */
extern int getopt ();
# ifndef __need_getopt
extern int getopt_long ();
extern int getopt_long_only ();

extern int _getopt_internal ();
# endif
#endif /* __STDC__ */

#ifdef	__cplusplus
}
#endif

/* Make sure we later can get all the definitions and declarations.  */
#undef __need_getopt

#endif /* getopt.h */
/***************************************************************************
                          help.cpp  -  description
                             -------------------
    begin                : Die Apr 23 2002
    copyright            : (C) 2002 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "help.h"

namespace Help
  {

/** gibt Hilfetext auf Konsole aus */

 void printHelp(const std::string & helpFilePath)
 {
    std::ifstream helpFile(helpFilePath.c_str());
    std::string line;
    if (helpFile){
       while (getline(helpFile, line))
        std::cout << line << "\n";
      helpFile.close();
    }
    else {
     std::cerr <<"highlight: Could not read "<< helpFilePath << "\n";
    }
 }

}
/***************************************************************************
                          help.h  -  description
                             -------------------
    begin                : Die Apr 23 2002
    copyright            : (C) 2002 by AndÃ© Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HELP_H
#define HELP_H

#include <iostream>
#include <fstream>
#include <string>

/**\ brief COntains methods for printing help messages
  *@author Andre Simon
  */
namespace Help
  {
    /** print help message to stdout */
     void printHelp(const std::string &);
  }

#endif
/***************************************************************************
                          htmlcode.cpp  -  description
                             -------------------
    begin                : Wed Nov 28 2001
    copyright            : (C) 2001 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "htmlgenerator.h"

using namespace std;

namespace highlight {


HtmlGenerator::HtmlGenerator(void)
{}

string  HtmlGenerator::formatStyleAttributes(const string & elemName,
                                             const ElementStyle & elem)
{
  ostringstream s;
  s  << "."<<elemName<<"\t{ color:#"
     << (elem.getColour().getHexRedValue())
     << (elem.getColour().getHexGreenValue())
     << (elem.getColour().getHexBlueValue()  )
     << ( elem.isBold() ?"; font-weight:bold" :"" )
     << ( elem.isItalic() ?"; font-style:italic" :"" )
     << ( elem.isUnderline() ?"; text-decoration:underline" :"" )
     << "; }\n" ;
  return  s.str();
}

string  HtmlGenerator::getOpenTag(const string& styleName ){
  return "<span class=\""+styleName+"\">";
}

HtmlGenerator::HtmlGenerator (
  const string &cssStyle,
  const string &enc,
  bool omitEnc,
  bool withAnchors)
    : CodeGenerator( cssStyle),
    brTag("<br>"),
    hrTag("<hr>"),
    idAttr("name"),
    fileSuffix(".html"),
    encoding(enc),
    omitEncoding(omitEnc),
    HTML_FOOTER(
      "\n</body>\n</html>\n<!--HTML generated by highlight "
      HIGHLIGHT_VERSION
      ", "
      HIGHLIGHT_URL
      "-->\n"),
    attachAnchors(withAnchors)
{
  styleTagOpen.push_back("");
  styleTagOpen.push_back(getOpenTag("str"));
  styleTagOpen.push_back(getOpenTag("num"));
  styleTagOpen.push_back(getOpenTag("slc"));
  styleTagOpen.push_back(getOpenTag("com"));
  styleTagOpen.push_back(getOpenTag("esc"));
  styleTagOpen.push_back(getOpenTag("dir"));
  styleTagOpen.push_back(getOpenTag("dstr"));
  styleTagOpen.push_back(getOpenTag("line"));
  styleTagOpen.push_back(getOpenTag("sym"));

  styleTagClose.push_back("");
  for (int i=1;i<NUMBER_BUILTIN_STYLES; i++) {
    styleTagClose.push_back("</span>");
  }

  /*assert (styleTagOpen.size()==styleTagClose.size());
  assert (styleTagOpen.size()==NUMBER_BUILTIN_STYLES);
*/
  newLineTag = "\n";
  spacer = " ";
  styleCommentOpen="/*";
  styleCommentClose="*/";
}

string HtmlGenerator::getStyleDefinition()
{
  if (styleDefinitionCache.empty()){
    ostringstream os;
    os << "body.hl\t{ background-color:#"
       << (docStyle.getBgColour().getHexRedValue())
       << (docStyle.getBgColour().getHexGreenValue())
       << (docStyle.getBgColour().getHexBlueValue())
       << "; }\n";
    os << "pre.hl\t{ color:#"
       << (docStyle.getDefaultStyle().getColour().getHexRedValue())
       << (docStyle.getDefaultStyle().getColour().getHexGreenValue())
       << (docStyle.getDefaultStyle().getColour().getHexBlueValue()  )
       << "; background-color:#"
       << (docStyle.getBgColour().getHexRedValue())
       << (docStyle.getBgColour().getHexGreenValue())
       << (docStyle.getBgColour().getHexBlueValue())
       << "; font-size:"
       << docStyle.getFontSize()
       << "pt; font-family:Courier;}\n";
    os << formatStyleAttributes("num", docStyle.getNumberStyle())
       << formatStyleAttributes("esc", docStyle.getEscapeCharStyle())
       << formatStyleAttributes("str", docStyle.getStringStyle())
       << formatStyleAttributes("dstr", docStyle.getDirectiveStringStyle())
       << formatStyleAttributes("slc", docStyle.getSingleLineCommentStyle())
       << formatStyleAttributes("com", docStyle.getCommentStyle())
       << formatStyleAttributes("dir", docStyle.getDirectiveStyle())
       << formatStyleAttributes("sym", docStyle.getSymbolStyle())
       << formatStyleAttributes("line", docStyle.getLineStyle());

    KeywordStyles styles = docStyle.getKeywordStyles();
    for (KSIterator it=styles.begin(); it!=styles.end(); it++){
      os << formatStyleAttributes(it->first, *(it->second));
    }
    styleDefinitionCache=os.str();
  }
  return styleDefinitionCache;
}

string HtmlGenerator::getHeader(const string &title)
{
  ostringstream os;
  os << getHeaderStart((title.empty())?"Source file":title );
  if (langInfo.getSyntaxHighlight())
  {
    if (includeStyleDef)    //CSS-Definition in HTML-<head> einfuegen
      {
        os << "<style type=\"text/css\">\n";
        os << "<!--\n";
        os << getStyleDefinition();
        os << CodeGenerator::readUserStyleDef();
        os << "//-->\n";
        os << "</style>" << endl;
      }
    else  //Referenz auf CSS-Datei einfuegen
      {
        os << "<link rel=\"stylesheet\" type=\"text/css\" href=\""
           << getStyleOutputPath()
           << "\""
           << ">\n";
      }
  }
  os << "</head>\n<body class=\"hl\">\n<pre class=\"hl\">";
  return os.str();
}

string HtmlGenerator::getFooter()
{
  return "</pre>" + HTML_FOOTER;
}


void HtmlGenerator::printBody()
{
  processRootState();
}



string HtmlGenerator::maskCharacter(unsigned char c)
{
  switch (c) {
    case '<' :
      return "&lt;";
      break;
    case '>' :
      return "&gt;";
      break;
    case '&' :
      return "&amp;";
      break;
    case '\"' :
      return "&quot;";
      break;

    case '@' :
      return "&#64;";
      break;

    default :
      string m;
      return m += c;
    }
}

void HtmlGenerator::insertLineNumber (bool insertNewLine)
{
  if (insertNewLine){
    //*out << getNewLine();
    wsBuffer += getNewLine();
  }
  if (showLineNumbers) {
    ostringstream numberPrefix;
    if (attachAnchors) {
      numberPrefix << "<a "
                   << idAttr
                   << "=\"l_"
                   << lineNumber
                   << "\">";
    }
    ostringstream os;
    if (lineNumberFillZeroes) os.fill('0');
    os <<setw(LINE_NUMBER_WIDTH)<<right<< lineNumber;
    numberPrefix<< styleTagOpen[LINENUMBER]
                << os.str()
                << spacer
                << styleTagClose[LINENUMBER];

    if (attachAnchors) {
       numberPrefix << "</a>";
    }

    wsBuffer += numberPrefix.str();
  }
}

string HtmlGenerator::getHeaderStart(const string &title){
    ostringstream header;
    header<<  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">"
            << "\n<html>\n<head>\n";
    if (!omitEncoding){
        header << "<meta http-equiv=\"content-type\" content=\"text/html; charset="<<encoding<<"\">\n";
    }
    header << "<title>" << title <<"</title>\n";
    return header.str();
}

bool HtmlGenerator::printIndexFile(const vector<string> &fileList,
                                   const string &outPath ){
  string suffix = fileSuffix;
  string outFilePath = outPath + "index" + suffix;
  ofstream indexfile(outFilePath.c_str());

  if (!indexfile.fail()){
     string inFileName;
     string inFilePath, newInFilePath;
     indexfile << getHeaderStart("Source Index" );
     indexfile << "</head>\n<body>\n<h1> Source Index</h1>\n"
               << hrTag
               <<  "\n<ul>\n";
     string::size_type pos;
     for (unsigned int i=0; i < fileList.size();  i++){
         pos=(fileList[i]).find_last_of(Platform::pathSeparator);
         if (pos!=string::npos){
             newInFilePath = (fileList[i]).substr(0, pos+1);
         } else {
            newInFilePath=Platform::pathSeparator;
         }
         if (newInFilePath!=inFilePath){
           indexfile << "</ul>\n<h2>";
           indexfile << newInFilePath;
           indexfile << "</h2>\n<ul>\n";
           inFilePath=newInFilePath;
         }
         inFileName = (fileList[i]).substr(pos+1);
         indexfile << "<li><a href=\"" << inFileName << suffix << "\">";
         indexfile << inFileName << suffix <<"</a></li>\n";
     }

     indexfile << "</ul>\n"
               << hrTag << brTag
               << "<small>Generated by highlight "
               << HIGHLIGHT_VERSION
               << ", <a href=\"" << HIGHLIGHT_URL << "\" target=\"new\">"
               << HIGHLIGHT_URL << "</a></small>";
     indexfile << HTML_FOOTER;
  } else {
    return false;
  }
  return true;
}

string HtmlGenerator::getMatchingOpenTag(unsigned int styleID){
  return getOpenTag(langInfo.getKeywordClasses()[styleID]);
 }

string HtmlGenerator::getMatchingCloseTag(unsigned int styleID){
  return "</span>";
}

}
/***************************************************************************
                          htmlgenerator.h  -  description
                             -------------------
    begin                : Wed Nov 28 2001
    copyright            : (C) 2001 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


#ifndef HTMLGENERATOR_H
#define HTMLGENERATOR_H

#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

#include "codegenerator.h"
#include "version.h"
#include "stylecolour.h"
#include "elementstyle.h"
#include "platform_fs.h"

namespace highlight {

/**
   \brief This class generates HTML.

   It contains information about the resulting document structure (document
   header and footer), the colour system, white space handling and text
   formatting attributes.

* @author Andre Simon
*/

class HtmlGenerator  : public highlight::CodeGenerator
  {
  public:

    /** Constructor
     \param colourTheme Name of Colour theme to use
     \param enc encoding name
     \param omitEnc switch to omit encoding information
     \param withAnchors Test if HTML anchors should be attached to line numbers
    */
    HtmlGenerator(const string &colourTheme,
                  const string &enc,
                  bool omitEnc=false,
                  bool withAnchors = false);

    HtmlGenerator();

    /** Destructor*/
    virtual ~HtmlGenerator() {};

    /** insert line number in the beginning of the new line
    */
    virtual void insertLineNumber(bool insertNewLine=true);

    /** Print document header
       \param  title Title of the document
    */
    string getHeader(const string &title);

    /** Print document body*/
    void printBody();

    /** Print document footer*/
    string getFooter();

    /** Print style definitions to external file
     \param outFile Path of external style definition
     */
     bool printExternalStyle(const string &outFile);

    /** Print index file with all input file names
      \param fileList List of output file names
      \param outPath Output path
    */
     bool printIndexFile(const vector<string> & fileList, const string &outPath);

  protected:

    /** some strings which are similar in HTML and XHTML*/
    string brTag, hrTag, idAttr, fileSuffix;

    /** Output encoding name */
    string encoding;

    /** switch to omit encoding name in file header */
    bool omitEncoding;

    /** HTML footer */
    string HTML_FOOTER;

    /** caches style definition */
    string styleDefinitionCache;

    /** \return CSS definition */
    string  getStyleDefinition();

    /** \return Content of user defined style file */
    string readUserStyleDef();

    /** \param title Dociment title
        \return Start of file header */
    virtual string getHeaderStart(const string &title);

  private:

    /**  \param styleName Style name
         \return Opening tag of the given style
    */
    string getOpenTag(const string& styleName);

    /** \return escaped character*/
    virtual string maskCharacter(unsigned char );

    /** test if anchors should be appied to line numbers*/
    bool attachAnchors;

    /**\return text formatting attributes in HTML format */
    string  formatStyleAttributes(const string & elemName, const ElementStyle & elem);

    /**  \param styleID Style ID
         \return Opening tag of the given style
    */
    string getMatchingOpenTag(unsigned int styleID);

    /**  \param styleID Style ID
         \return Closing tag of the given style
    */
    string getMatchingCloseTag(unsigned int styleID);
  };

}

#endif
/***************************************************************************
                          languagedefinition.cpp  -  description
                             -------------------
    begin                : Wed Nov 28 2001
    copyright            : (C) 2001 by Andre imon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "languagedefinition.h"

using namespace std;

namespace highlight {

LanguageDefinition::LanguageDefinition():
    ignoreCase(false),
    disableHighlighting(false),
    allowExtEscape(false),
    vhdl_mode(false),
    java_mode(false),
    allowNestedComments(true),
    fullLineComment(false),
    reformatCode(false)
{}

int LanguageDefinition::isKeyword(const string &s)
{
  if (s.length())
    {
      if (keywords.count(s)){
        return keywords[s];
      }
      else if (prefixes.count(s[0])){
        return prefixes[s[0]];
      }
    }
  return 0;
}

bool LanguageDefinition::isPrefix(unsigned char c)
{
  return ( prefixes.count(c));
}

void LanguageDefinition::addSimpleSymbol(stringstream& symbolStream,
                                        State state,
                                        const string& paramValues ) {
  istringstream valueStream(paramValues);
  bool valExists=false;
  string value;
   while (valueStream >> value)
    {
      symbolStream << " " << value;
      valExists = true;
    }
  if (valExists)
    {
      symbolStream << " " << state;
    }
}

void LanguageDefinition::addDelimiterSymbol(stringstream& symbolStream,
                                        State stateBegin, State stateEnd,
                                        const string& paramValues,
                                        unsigned int classID) {
  istringstream valueStream(paramValues);
  string delimPrefix, delimSuffix;
  while (valueStream>>delimPrefix){
    valueStream >> delimSuffix;
    symbolStream << " "<<delimPrefix <<" " << stateBegin;
    symbolStream <<" "<< delimSuffix<<" "<< stateEnd;
    delimiterPrefixes.insert(make_pair(delimPrefix, classID));
  };
}

bool LanguageDefinition::getFlag( string& paramValue){
      return (StringTools::lowerCase(paramValue)=="true");
}

unsigned char LanguageDefinition::getSymbol(const string& paramValue){
      istringstream valueStream(paramValue);
      unsigned char symbol;
      valueStream >> symbol;
      return symbol;
}

void LanguageDefinition::addKeywords(const string &kwList,
                                     int classID){
  istringstream valueStream(kwList);
  string keyword;
  while (valueStream >> keyword){
    keywords.insert(make_pair(keyword, classID));
  }
}

unsigned int LanguageDefinition::generateNewKWClass(const string& newClassName){
  unsigned int newClassID=0;
  bool found=false;
  while (newClassID<keywordClasses.size() && !found){
   found= (newClassName==keywordClasses[newClassID++]);
  }
  if (!found){
   newClassID++;
   keywordClasses.push_back(newClassName);
  }
  return newClassID;
}

unsigned int LanguageDefinition::getDelimPrefixClassID(const string& prefix){
  if (delimiterPrefixes.count(prefix)){
    return delimiterPrefixes[prefix];
  }
  return 0;
}

bool LanguageDefinition::load(const string& langDefPath, bool clear)
{
  if (clear)  reset();

  ConfigurationReader langDef(langDefPath);
  if (langDef.found())
    {
      currentPath=langDefPath;
      disableHighlighting=false;
      string token;
      stringstream symbolStrStream;

      //Stringstream zum Einlesen der Token:
      istringstream valueStream;

      addDelimiterSymbol(symbolStrStream, ML_COMMENT_BEGIN, ML_COMMENT_END,
                         langDef.getParameter("ml_comment"));

      addSimpleSymbol(symbolStrStream, SL_COMMENT,
                      langDef.getParameter("sl_comment"));

      addSimpleSymbol(symbolStrStream, ESC_CHAR,
                      langDef.getParameter("escchar"));

      addSimpleSymbol(symbolStrStream, DIRECTIVE_LINE,
                      langDef.getParameter("directive"));

      addSimpleSymbol(symbolStrStream, DIRECTIVE_LINE_END,
                      langDef.getParameter("directiveend"));

      addSimpleSymbol(symbolStrStream, STRING,
                      langDef.getParameter("stringdelimiters"));

      ignoreCase=getFlag(langDef.getParameter("ignorecase"));
      allowNestedComments=getFlag(langDef.getParameter("allownestedcomments"));
      vhdl_mode=getFlag(langDef.getParameter("vhdl_mode"));
      java_mode=getFlag(langDef.getParameter("java_mode"));
      disableHighlighting=getFlag(langDef.getParameter("disablehighlighting"));
      fullLineComment=getFlag(langDef.getParameter("fl_comment"));
      reformatCode=getFlag(langDef.getParameter("reformatting"));
      rawStringPrefix=getSymbol(langDef.getParameter("rawstringprefix"));
      continuationChar=getSymbol(langDef.getParameter("continuationsymbol"));
      allowExtEscape=getFlag(langDef.getParameter("allowextescape"));

      string paramName, className, classValue;
      vector<string> paramNames=langDef.getParameterNames();
      for (unsigned int i=0;i<paramNames.size();i++){
         paramName=paramNames[i];
         className=StringTools::getParantheseVal(paramName);
         classValue=langDef.getParameter(paramName);
         if (paramName.find("kw_list") != string::npos ){
           addKeywords(classValue, generateNewKWClass(className));
         }
         if (paramName.find("kw_prefix") != string::npos){
           prefixes.insert(make_pair(classValue[0], generateNewKWClass(className)));
         }
         if (paramName.find("kw_delim") != string::npos ){
          addDelimiterSymbol(symbolStrStream, KEYWORD_BEGIN, KEYWORD_END,
                             classValue, generateNewKWClass(className));
        }
         if (paramName.find("tag_delim") != string::npos ){
          addDelimiterSymbol(symbolStrStream, TAG_BEGIN, TAG_END,
                             classValue, generateNewKWClass(className));
        }
      }

      // zuletzt einlesen, um Probleme mit Delimitern, die Zeichen der
      // Symbolliste enthalten, zu vermeiden
      addSimpleSymbol(symbolStrStream, SYMBOL, langDef.getParameter("symbols"));

      valueStream.str(langDef.getParameter("allowedchars"));
      while (valueStream >> token )
        {
          allowedChars += token;
        }
      symbolString = symbolStrStream.str();

      string fileToInclude=langDef.getParameter("include");
      if (!fileToInclude.empty()){
        string::size_type Pos = langDefPath.find_last_of(Platform::pathSeparator);
        string includeLangDefPath = langDefPath.substr(0, Pos+1) + fileToInclude;
        load(includeLangDefPath, false);
      }
      return true;
    }
  else
    {
      currentPath.clear();
      return false;
    }
}

void LanguageDefinition::reset()
{
  keywords.clear();
  keywordClasses.clear();
  delimiterPrefixes.clear();;
  prefixes.clear();
  allowedChars.clear();
  ignoreCase= false;
  java_mode= vhdl_mode= false;
  allowNestedComments= reformatCode = false;
  rawStringPrefix = continuationChar = '\0';
  disableHighlighting=false;
  fullLineComment=false;
}

bool LanguageDefinition::isVHDL()
{
  return vhdl_mode;
}

bool LanguageDefinition::isJava()
{
  return java_mode;
}

bool LanguageDefinition::allowNestedMLComments(){
  return allowNestedComments;
}

bool LanguageDefinition::highlightingDisabled(){
  return disableHighlighting;
}

bool LanguageDefinition::isFullLineComment(){
  return fullLineComment;
}

bool LanguageDefinition::needsReload(const string &langDefPath){
  return currentPath!=langDefPath;
}

bool LanguageDefinition::enableReformatting(){
  return reformatCode;
}

const KeywordMap& LanguageDefinition::getKeywords() const{
  return keywords;
}

string &LanguageDefinition::getSymbolString() {
  return symbolString;
}

unsigned char LanguageDefinition::getRawStringPrefix(){
  return rawStringPrefix;
}

unsigned char LanguageDefinition::getContinuationChar(){
  return continuationChar;
}

string &LanguageDefinition::getAllowedChars() {
  return allowedChars;
}

bool LanguageDefinition::getSyntaxHighlight() {
  return !disableHighlighting;
}

bool LanguageDefinition::isIgnoreCase() {
  return ignoreCase;
}

const vector<string>&LanguageDefinition::getKeywordClasses() const{
  return keywordClasses;
}

bool LanguageDefinition::allowExtEscSeq() {
    return allowExtEscape;
}

}
/***************************************************************************
                          languagedefinition.h  -  description
                             -------------------
    begin                : Wed Nov 28 2001
    copyright            : (C) 2001 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef LANGUAGEDEFINITION_H
#define LANGUAGEDEFINITION_H

#include <string>
#include <map>
#include <iostream>
#include <fstream>
#include <iterator>
#include <sstream>

#include "configurationreader.h"
//#include "stringtools.h"
#include "platform_fs.h"
#include "enums.h"


namespace highlight {

/** maps keywords and the corresponding class IDs*/
typedef map <string, int> KeywordMap;

/** maps keyword prefixes and the corresponding class IDs*/
typedef map <unsigned char, int> PrefixMap;

/**\brief Contains specific data of the programming language being processed.

   The load() method will only read a new language definition if the given
   file path is not equal to the path of the current language definition.

* @author Andre  Simon
*/

class LanguageDefinition  {

  public:

    LanguageDefinition();

    /**\return Symbol string, containg all known symbols with the referencing state ids*/
    string &getSymbolString();

    /** \return Prefix of raw strings */
    unsigned char getRawStringPrefix();

   /** \return Continuation Character */
    unsigned char getContinuationChar();

    /** \return List of characters allowed within identifiers */
    string &getAllowedChars();

    /** \return true if syntax highlighting is enabled*/
    bool getSyntaxHighlight();

    /** \return True if language is case sensitive */
    bool isIgnoreCase();

    /** \param s String
         \return class id of keyword, 0 if s is not a keyword */
    int isKeyword(const string &s);


    /** \return true if c is member of prefix list*/
    bool isPrefix(unsigned char c);

    /** Load new language definition
        \param langDefPath Path of language definition
        \param clear Test if former data should be deleted
        \return True if successfull  */
    bool load(const string& langDefPath, bool clear=true);

    /** \return  True if programming language is VHDL */
    bool isVHDL();

    /** \return  True if programming language is Java */
    bool isJava();

    /** \return True if multi line comments may be nested */
    bool allowNestedMLComments();

    /** \return True if highlighting is disabled */
    bool highlightingDisabled();

    /** \return True if single line comments must start at coloumn 1 */
    bool isFullLineComment();

    /** \return True the next load() call will load a new language definition
        \param  langDefPath Path to language definition  */
    bool needsReload(const string &langDefPath);

    /** \return True if current language may be reformatted (c, c++, c#, java) */
    bool enableReformatting();

    /** \return True if escape sequences are allowed outsde of strings */
    bool allowExtEscSeq();

    /** \return Class ID of given keyword delimiter prefix
        \param prefix Keyword delimiter prefix   */
    unsigned int getDelimPrefixClassID(const string& prefix);

    /** \return keywords*/
    const KeywordMap& getKeywords() const;

    /** \return keyword classes*/
    const vector<string>& getKeywordClasses() const;

  private:
    // string containing symbols and their IDs of the programming language
    string symbolString;

    // string with special characters that may occour in keywords
    string allowedChars;

    // path to laoed language definition
    string currentPath;

    KeywordMap keywords;

    vector <string> keywordClasses;

    KeywordMap delimiterPrefixes;

    PrefixMap prefixes;

    // keywords are not case sensitive if set
    bool ignoreCase,
    disableHighlighting,
    allowExtEscape,

    // switch to enable VHDL workarounds
    vhdl_mode,

    // switch to enable Java workarounds
    java_mode,

    // allow nested multi line comment blocks
    allowNestedComments,

    // single line comments have to start in coloumn 1 if set
    fullLineComment,

    // code formatting is enabled if set
    reformatCode;

    // Character, die eine Variable bzw. ein Keyword kennzeichnen
    unsigned char rawStringPrefix,
                  continuationChar;

    /** setzt Membervariablen auf Defaultwerte */
    void reset();

    // add a symbol sequencs to the symbolStream
    void addSimpleSymbol(stringstream& symbolStream, State state,
                         const string& paramValues );

    // add a delimiter symbol sequencs to the symbolStream
    void addDelimiterSymbol(stringstream& symbolStream,
                            State stateBegin, State stateEnd,
                            const string& paramValues,
                            unsigned int classID=0);

    bool getFlag( string& paramValue);

    unsigned char getSymbol(const string& paramValue);

    // generate a unique class ID if the class name
    unsigned int generateNewKWClass(const string& newClassName);

    // add keywords to the given class
    void addKeywords(const string &kwList, int classID);

  };

}
#endif
/***************************************************************************
                          LatexCode.cpp  -  description
                             -------------------
    begin                : Mit Jul 24 2002
    copyright            : (C) 2002 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "latexgenerator.h"

namespace highlight {

LatexGenerator::LatexGenerator(const string &colourTheme,
                               bool replQuotes)
    : CodeGenerator(colourTheme),
      replaceQuotes(replQuotes)
{
  styleTagOpen.push_back( "\\hlstd{");
  styleTagOpen.push_back( "\\hlstr{");
  styleTagOpen.push_back( "\\hlnum{");
  styleTagOpen.push_back( "\\hlslc{");
  styleTagOpen.push_back( "\\hlcom{");
  styleTagOpen.push_back( "\\hlesc{");
  styleTagOpen.push_back( "\\hldir{");
  styleTagOpen.push_back( "\\hldstr{");
  styleTagOpen.push_back( "\\hlline{");
  styleTagOpen.push_back( "\\hlsym{");

  for (int i=0;i<NUMBER_BUILTIN_STYLES; i++){
   styleTagClose.push_back( "}");
  }

  // avoid "Underfull \hbox (badness 10000)" warnings
  newLineTag = "\\\\\n";
  longLineTag = "\\hspace*{\\fill}" + newLineTag;

  spacer = "\\ ";

  maskWs=true;
  maskWsBegin = "\\hlstd{";
  maskWsEnd = "}";

  excludeWs=true;

  styleCommentOpen="%";
}

LatexGenerator::LatexGenerator()
{}
LatexGenerator::~LatexGenerator()
{}

string LatexGenerator::formatStyleAttributes(const string & elemName,
                                             const ElementStyle &elem)
{
  ostringstream s;
  s  << "\\newcommand{\\hl"
     << elemName
     << "}[1]{\\textcolor[rgb]{"
     << elem.getColour().getLatexRedValue() << ","
     << elem.getColour().getLatexGreenValue() << ","
     << elem.getColour().getLatexBlueValue()
     << "}{";

  if (elem.isBold())
    s << "\\bf{";
  if (elem.isItalic())
    s << "\\it{";

  s  <<"#1";

  if (elem.isBold())
    s << "}";
  if (elem.isItalic())
    s << "}";

  s  <<"}}\n";
  return s.str();
}

void LatexGenerator::printBody()
{
  *out << "\\noindent\n"
       << "\\ttfamily\n";

  processRootState();

  *out << "\\mbox{}\n"
       << "\n\\normalfont\n";
}

string LatexGenerator::getHeader(const string & title)
{
  ostringstream os;
  os << "\\documentclass{article}\n"
     << "\\usepackage{color}\n"
     << "\\usepackage{alltt}\n";

  if (langInfo.getSyntaxHighlight()) {
    if (includeStyleDef) {
      os << "\n"<<getStyleDefinition();
      os << CodeGenerator::readUserStyleDef();
    }  else  {
      os << "\n\\input {"
         << getStyleOutputPath()
         << "}\n";
    }
  }

  os << "\n\\title{" << title << "}\n"
     << "\\begin{document}\n"
     << "\\pagecolor{bgcolor}\n";
  return os.str();
}

string LatexGenerator::getFooter()
{
  ostringstream os;
  os << "\\end {document}\n"
     << "(* LaTeX generated by highlight "
     << HIGHLIGHT_VERSION
     << ", "
     << HIGHLIGHT_URL
     << " *)\n";
  return os.str();
}

string LatexGenerator::getNewLine(){
    return (showLineNumbers)? newLineTag:longLineTag;
}

string LatexGenerator::maskCharacter(unsigned char c)
{
  switch (c)
    {
    case '<' :
      return "$<$";
      break;
    case '>' :
      return "$>$";
      break;
    case '{':
    case '}':
    case '&':
    case '$':
    case '#':
    case '%':
      {
       string m;
       m  ="\\";
       m += c;
       return m;
      }
      break;
    case '\"':
      return (fragmentOutput && replaceQuotes)?"\\dq{}":"\"";
      break;
    case '_':
      return "\\textunderscore ";
      break;
    case '^':
      return "\\textasciicircum ";
      break;
    case '\\':
      return "$\\backslash$";
      break;
    case '~':
      return "$\\sim$";
      break;
    case '|':
      return "\\textbar ";
      break;
    // avoid latex compilation failure if [ or * follows a line break (\\)
    case '*':
    case '[':
    case ']':
    // avoid "merging" of consecutive '-' chars when included in bold font ( \bf )
    case '-':
      {
       string m;
       m= "{";
       m+= c;
       m+= "}";
       return m;
      }
      break;
    case ' ':
      return spacer;
      break;
    case AUML_LC:
      return "\\\"a";
      break;
    case OUML_LC:
      return "\\\"o";
      break;
    case UUML_LC:
      return "\\\"u";
      break;
    case AUML_UC:
      return "\\\"A";
      break;
    case OUML_UC:
      return "\\\"O";
      break;
    case UUML_UC:
      return "\\\"U";
      break;
    case AACUTE_LC:
      return "\\'a";
      break;
    case EACUTE_LC:
      return "\\'e";
      break;
    case OACUTE_LC:
      return "\\'o";
      break;
    case UACUTE_LC:
      return "\\'u";
      break;
    case AGRAVE_LC:
      return "\\`a";
      break;
    case EGRAVE_LC:
      return "\\`e";
      break;
    case OGRAVE_LC:
      return "\\`o";
      break;
    case UGRAVE_LC:
      return "\\`u";
      break;
    case AACUTE_UC:
      return "\\'A";
      break;
    case EACUTE_UC:
      return "\\'E";
      break;
    case OACUTE_UC:
      return "\\'O";
      break;
    case UACUTE_UC:
      return "\\'U";
      break;
    case AGRAVE_UC:
      return "\\`A";
      break;
    case EGRAVE_UC:
      return "\\`E";
      break;
    case UGRAVE_UC:
      return "\\`O";
      break;
    case OGRAVE_UC:
      return "\\`U";
      break;
    case SZLIG:
      return "\\ss ";
      break;
 /*    #ifndef _WIN32
 // skip  first byte of multibyte chracters
    case 195:
      return string("");
      break;
#endif*/

    default :
      {
      string m;
      return m+=c;
     }
    }
}

string LatexGenerator::getMatchingOpenTag(unsigned int styleID){
  return "\\hl"+langInfo.getKeywordClasses()[styleID]+"{";
 }

string LatexGenerator::getMatchingCloseTag(unsigned int styleID){
  return "}";
}


string LatexGenerator::getStyleDefinition()
{
    if (styleDefinitionCache.empty()){
        ostringstream os;
        os << formatStyleAttributes("std", docStyle.getDefaultStyle());
        os << formatStyleAttributes("num", docStyle.getNumberStyle());
        os << formatStyleAttributes("esc", docStyle.getEscapeCharStyle());
        os << formatStyleAttributes("str", docStyle.getStringStyle());
        os << formatStyleAttributes("dstr", docStyle.getDirectiveStringStyle());
        os << formatStyleAttributes("slc", docStyle.getSingleLineCommentStyle());
        os << formatStyleAttributes("com", docStyle.getCommentStyle());
        os << formatStyleAttributes("dir", docStyle.getDirectiveStyle());
        os << formatStyleAttributes("sym", docStyle.getSymbolStyle());
        os << formatStyleAttributes("line", docStyle.getLineStyle());

        KeywordStyles styles = docStyle.getKeywordStyles();
        for (KSIterator it=styles.begin(); it!=styles.end(); it++){
            os << formatStyleAttributes(it->first, *(it->second));
        }
        os << "\\definecolor{bgcolor}{rgb}{"
                << docStyle.getBgColour().getLatexRedValue() << ","
                << docStyle.getBgColour().getLatexGreenValue() << ","
                << docStyle.getBgColour().getLatexBlueValue()
                << "}\n";
        os << "\\oddsidemargin -3mm\n\\textwidth 165,2truemm\n"
                << "\\topmargin 0truept\n\\headheight 0truept\n"
                << "\\headsep 0truept\n\\textheight 230truemm\n";

        styleDefinitionCache=os.str();
    }
    return styleDefinitionCache;
}


}
/***************************************************************************
                          latexgenerator.h  -  description
                             -------------------
    begin                : Mit Jul 24 2002
    copyright            : (C) 2002 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef LATEXGENERATOR_H
#define LATEXGENERATOR_H

#include <string>
#include <iostream>
#include <sstream>

#include "codegenerator.h"
#include "version.h"
#include "charcodes.h"


namespace highlight {

/**
   \brief This class generates LaTeX.

   It contains information about the resulting document structure (document
   header and footer), the colour system, white space handling and text
   formatting attributes.

* @author Andre Simon
*/

class LatexGenerator : public highlight::CodeGenerator
  {
  public:

   /** Constructor
     \param colourTheme Name of Colour theme to use
     \param replQuotes Test if quotes shold be replaced by \ dq
    */
    LatexGenerator(const string &colourTheme,
                   bool replQuotes=false);
    LatexGenerator();
    ~LatexGenerator();

   /** prints document header
       \param  title Title of the document
    */
    string getHeader(const string & title);

    /** Prints document footer*/
    string getFooter();

    /** Prints document body*/
    void printBody();

  private:

     string styleDefinitionCache;
     string longLineTag;

    /** \return escaped character*/
    virtual string maskCharacter(unsigned char );

    /**\return text formatting attributes in LaTeX format */
    string formatStyleAttributes(const string & elemName,
                                 const ElementStyle & elem);

    /** test if double quotes should be replaced by \dq{} */
    bool replaceQuotes;

    string getNewLine();

    string getStyleDefinition();

    string getMatchingOpenTag(unsigned int styleID);
    string getMatchingCloseTag(unsigned int styleID);
  };

}

#endif
/***************************************************************************
                          main.cpp  -  description
                             -------------------
    begin                : Die Apr 23 22:16:35 CEST 2002
    copyright            : (C) 2002-2004 by André Simon
    email                : andre.simon1@gmx.de


   Highlight is a universal source code to HTML converter. Syntax highlighting
   is formatted by Cascading Style Sheets. It's possible to easily enhance
   highlight's parsing database.

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "main.h"

using namespace std;

void HighlightApp::printVersionInfo()
{
  cout << "\n highlight version "
       << HIGHLIGHT_VERSION
       << "\n Copyright (C) 2002-2005 Andre Simon <andre.simon1@gmx.de>"
       << "\n\n Artistic Style Classes (1.15.3)"
       << "\n Copyright (C) 1998-2002 Tal Davidson <davidsont@bigfoot.com>"
       << "\n\n Dirstream Classes (0.4)"
       << "\n Copyright (C) 2002-2004 Benjamin Kaufmann <hume@c-plusplus.de>"
       << "\n\n This software is released under the terms of the GNU General "
       << "Public License."
       << "\n For more information about these matters, see the file named "
       << "COPYING.\n\n";
       #ifdef USE_LOCAL_GETOPT
         cout   << " (Built with USE_LOCAL_GETOPT flag set.)\n";
       #endif
       #ifdef HL_DATA_DIR
        cout    << " (HL_DATA_DIR: \"" <<HL_DATA_DIR<< "\" )\n";
       #endif
}

void HighlightApp::printBadInstallationInfo()
{
  cerr << "highlight: Data directory not found. Bad installation or wrong "
       << OPT_DATADIR << " parameter."
       << "\n\nCopy the highlight files into one of the directories listed "
       << "in INSTALL.\nYou may also set the data directory with "
       << OPT_DATADIR << " and " << OPT_ADDDATADIR << ".\n";
}

bool HighlightApp::listInstalledFiles(bool showThemes)
{
  vector <string> filePaths;
  string wildcard=(showThemes)? "*.style":"*.lang";
  unsigned int suffixLength=wildcard.length()-1;

  string searchDir = ((showThemes) ? dataDir.getThemeDir():
                                     dataDir.getLangDefDir()) + wildcard;

  bool directoryOK = Platform::getDirectoryEntries(filePaths, searchDir, true);
  if (!directoryOK) {
    cerr << "highlight: Could not access directory "
         <<  searchDir
         << ", aborted.\n";
     return false;
  }

  cout << "\n  Installed "
       << ((showThemes)? "themes":"language definitions ")
       << "(located in "
       << ((showThemes)?dataDir.getThemeDir():dataDir.getLangDefDir())
       << ") :\n"
       << endl;

  sort(filePaths.begin(), filePaths.end());
  string  temp;

  for (unsigned int i=0;i< filePaths.size(); i++){
    if (showThemes)
      temp = (filePaths[i]).substr(dataDir.getThemeDir().length());
    else
      temp = (filePaths[i]).substr(dataDir.getLangDefDir().length());
    cout << "  "<<temp.substr(0, temp.length()- suffixLength) << endl;
  }
  cout <<"\n  Use name of the desired "
       << ((showThemes)?"theme":"language")
       << " with the --"
       << ((showThemes)? OPT_STYLE : OPT_SYNTAX)
       << " option.\n" << endl;
  return true;
}

void HighlightApp::printDebugInfo(highlight::LanguageDefinition &lang,
                                   const string & langDefPath)
{
   cerr << "\nLoading language definition: " << langDefPath;
   cerr << "\n\nSYMBOLS: "
        << lang.getSymbolString();
   cerr << "\n\nKEYWORDS: ";
   highlight::KeywordMap::iterator it;
   highlight::KeywordMap keys=lang.getKeywords();
   cerr << "\n\nID    Keyword \n";
   for (it=keys.begin(); it!=keys.end();it++){
      cerr << it->second
           << "  <- \""
           << it->first <<"\"\n";
   }
   cerr <<"\n";
}

string HighlightApp::getFileSuffix(const string &fileName) {
  size_t ptPos=fileName.rfind(".");
  return (ptPos == string::npos) ?
         "" : fileName.substr(ptPos+1, fileName.length());
}

bool HighlightApp::loadMapConfig(const string& name, StringMap* map){
    string extPath=dataDir.getDir() + name + ".conf";
    ConfigurationReader config(extPath);
    if (config.found() )
    {
        stringstream values;
        string paramName, paramVal;
        for (unsigned int i=0;i<config.getParameterNames().size();i++){
            paramName = config.getParameterNames()[i];
            values.str(config.getParameter(paramName)) ;
            while (values >> paramVal) {
                map->insert(make_pair( paramVal,  paramName));
            }
            values.clear();
        }
        return true;
    } else {
        cerr << "highlight: Configuration file "<< extPath << " not found.\n";
        return false;
    }
}


int HighlightApp::getNumDigits(int i){
  int res=0;
  while (i){
    i/=10;
    ++res;
  }
  return res;
}

void HighlightApp::printProgressBar(int total, int count){
  if (!total) return;
  int p=100*count / total;
  int numProgressItems=p/10;
  cout << "\r[";
  for (int i=0;i<10;i++){
    cout <<((i<numProgressItems)?"#":" ");
  }
  cout<< "] " <<setw(3)<<p<<"%, "<<count << " / " << total << "  " <<flush;
  if (p==100) {
    cout << endl;
  }
}

void HighlightApp::printCurrentAction(const string&outfilePath,
                                      int total, int count, int countWidth){
  cout << "Writing file "
       << setw(countWidth)<< count
       << " of "
       << total
       << ": "
       << outfilePath
       << "\n";
}

void HighlightApp::printIOErrorReport(unsigned int numberErrorFiles,
                                      vector<string> & fileList,
                                      const string &action){
  cerr << "highlight: Could not "
       << action
       << " file"
       << ((numberErrorFiles>1)?"s":"")<<":\n";
  copy (fileList.begin(), fileList.end(), ostream_iterator<string>(cerr, "\n"));
  if (fileList.size() < numberErrorFiles) {
    cerr << "... ["
         << (numberErrorFiles - fileList.size() )
         << " of "
         << numberErrorFiles
         << " failures not shown, use --"
         << OPT_VERBOSE
         << " switch to print all paths]\n";
  }
}

string HighlightApp::analyzeShebang(const string& file){
    if (scriptShebangs.empty()) loadMapConfig("scriptre", &scriptShebangs);
    ifstream inFile(file.c_str());
    string firstLine;
    getline (inFile, firstLine);
    return scriptShebangs[StringTools::trimRight(firstLine)];
}

string HighlightApp::guessFileType(const string& suffix, const string &inputFile)
{
    if (extensions.empty()) loadMapConfig("extensions", &extensions);
    string fileType = (extensions.count(suffix)) ? extensions[suffix] : suffix ;
    if (!fileType.empty()) return fileType;
    return analyzeShebang(inputFile);
}


int HighlightApp::run(int argc, char**argv){

  //get command line options
  CmdLineOptions options(argc, argv);

  // set data directory path, where /langDefs and /themes reside
  string highlightRootDir = Platform::getAppPath();

  // determine highlight data directory
  if (! dataDir.searchDataDir((options.dataDirGiven())?
                                options.getDataDir(): highlightRootDir)){
    printBadInstallationInfo();
    return EXIT_FAILURE;
  }

  if (options.additionalDataDirGiven()){
     dataDir.setAdditionalDataDir(options.getAdditionalDataDir());
  }

  if (options.printVersion()) {
    printVersionInfo();
    return EXIT_SUCCESS;
  }

  if  (options.printHelp())  {
    Help::printHelp(dataDir.getHelpMsgDir() + options.getHelpLang());
    return EXIT_SUCCESS;
  }

  if (options.showThemes() || options.showLangdefs()) {
    return listInstalledFiles(options.showThemes())?EXIT_SUCCESS:EXIT_FAILURE;
  }

  // list of input files
  const  vector <string> inFileList=options.getInputFileNames();

  string stylePath=dataDir.searchForTheme(options.getStyleName());

  highlight::CodeGenerator *generator =
    highlight::CodeGenerator::getInstance(options.getOutputType(),
                                          stylePath,
                                          options.getStyleInFilename(),
                                          options.getStyleOutFilename(),
                                          options.getCharSet(),
                                          options.includeStyleDef(),
                                          options.attachLineAnchors(),
                                          options.replaceQuotes(),
                                          options.fopCompatible(),
                                          options.getNumberSpaces(),
                                          options.getWrappingStyle(),
                                          options.printLineNumbers(),
                                          options.fillLineNrZeroes(),
                                          options.fragmentOutput(),
                                          options.omitEncodingName() );

  assert (generator!=NULL);

  bool styleFileWanted = !options.fragmentOutput() || options.styleOutPathDefined();

  if (!generator->styleFound() ) {
    cerr << "highlight: Could not find style "
         << stylePath
         << ".\n";
    highlight::CodeGenerator::deleteInstance();
    return EXIT_FAILURE;
  }

  if (!options.getIndentScheme().empty()){
    string indentSchemePath =
              dataDir.searchForIndentScheme(options.getIndentScheme()+".indent");
    if (!generator->initIndentationScheme(indentSchemePath)){
        cerr << "highlight: Could not find indentation scheme "
             << indentSchemePath
             << ".\n";
       highlight::CodeGenerator::deleteInstance();
       return EXIT_FAILURE;
    }
  }

  string outDirectory = options.getOutDirectory();
  if (!outDirectory.empty() && !options.quietMode() && !dirstr::directory_exists(outDirectory) ){
     cerr << "highlight: Output directory \""
          << outDirectory
	  << "\" does not exist.\n";
     return EXIT_FAILURE;
  }

  bool initError=false, IOError=false;

  if (    !options.includeStyleDef()
       && (styleFileWanted)
       && options.formatSupportsExtStyle()) {
      string cssOutFile=outDirectory  + options.getStyleOutFilename();
      bool success=generator->printExternalStyle (cssOutFile);
      if (!success){
          cerr << "highlight: Could not write " << cssOutFile <<".\n";
          IOError = true;
      }
  }

  if (options.printIndexFile()){
    bool success=generator -> printIndexFile(inFileList, outDirectory);
    if (!success){
      cerr << "highlight: Could not write index file.\n";
      IOError = true;
    }
  }

  unsigned int fileCount=inFileList.size(),
               fileCountWidth=getNumDigits(fileCount),
               i=0,
               numBadFormatting=0,
               numBadInput=0,
               numBadOutput=0;

  vector<string> badFormattedFiles, badInputFiles, badOutputFiles;
  string outFilePath;
  string suffix, lastSuffix;

  if (options.syntaxGiven()) {  // user defined language definition, valid for all files
      suffix = guessFileType(options.getLanguage());
  }

  while (i < fileCount && !initError) {
    if (!options.syntaxGiven()) {  // determine file type for each file
       suffix = guessFileType(getFileSuffix(inFileList[i]), inFileList[i]);
    }
    if (suffix.empty()) {
      if (!options.enableBatchMode() && !styleFileWanted)
        cerr << "highlight: Undefined language definition. Use --"
             << OPT_SYNTAX << " option.\n";
        if (!options.forceOutput()){
          initError = true;
          break;
        }
    }

    if (suffix != lastSuffix) {
        string langDefPath=dataDir.searchForLangDef(suffix+".lang");
        highlight::LoadResult loadRes= generator->initLanguage(langDefPath);
        if (loadRes==highlight::LOAD_FAILED){
            cerr << "highlight: Unknown source file extension \""
                << suffix
                << "\".\n";
            if (!options.forceOutput()){
              initError = true;
              break;
            }
        }
        if (options.printDebugInfo() && loadRes==highlight::LOAD_NEW){
            printDebugInfo(generator->getLanguage(), langDefPath);
        }
        lastSuffix = suffix;
    }

    if (options.enableBatchMode()){
      string::size_type pos=(inFileList[i]).find_last_of(Platform::pathSeparator);
      outFilePath = outDirectory;
      outFilePath += inFileList[i].substr(pos+1);
      outFilePath += options.getOutFileSuffix();

      if (!options.quietMode()) {
        if (options.printProgress()){
           printProgressBar(fileCount, i+1);
        } else {
           printCurrentAction(outFilePath, fileCount, i+1, fileCountWidth);
        }
      }
     } else {
        outFilePath = options.getSingleOutFilename();
     }

     highlight::ParseError error = generator->printOutput(inFileList[i], outFilePath);
     if (error==highlight::BAD_INPUT){
       if (numBadInput++ < IO_ERROR_REPORT_LENGTH || options.printDebugInfo()) {
         badInputFiles.push_back(inFileList[i]);
        }
     } else if (error==highlight::BAD_OUTPUT){
       if (numBadOutput++ < IO_ERROR_REPORT_LENGTH || options.printDebugInfo()) {
         badOutputFiles.push_back(outFilePath);
       }
     }
     if (options.formattingEnabled() && !generator->formattingIsPossible()){
      if (numBadFormatting++ < IO_ERROR_REPORT_LENGTH || options.printDebugInfo()) {
        badFormattedFiles.push_back(outFilePath);
      }
     }
     ++i;
    }

    if (numBadInput){
      printIOErrorReport(numBadInput, badInputFiles, "read input");
      IOError = true;
    }
    if (numBadOutput){
      printIOErrorReport(numBadOutput, badOutputFiles, "write output");
      IOError = true;
    }
    if (numBadFormatting){
      printIOErrorReport(numBadFormatting, badFormattedFiles, "reformat");
    }

    highlight::CodeGenerator::deleteInstance();
    return (initError || IOError) ? EXIT_FAILURE : EXIT_SUCCESS;
}


int main(int argc, char **argv) {
  HighlightApp app;
  return app.run(argc, argv);
}
//
// C++ Interface: main
//
// Description:
//
//
// Author: Andre Simon <andre.simon1@gmx.de>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef HIGHLIGHT_APP
#define HIGHLIGHT_APP


#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <iomanip>
#include <cassert>

#include "./dirstream0.4/dirstream.h"
#include "cmdlineoptions.h"
#include "configurationreader.h"
#include "codegenerator.h"
#include "help.h"
#include "datadir.h"
#include "version.h"
#include "platform_fs.h"

#define IO_ERROR_REPORT_LENGTH 5
#define SHEBANG_CNT 12

typedef map<string, string> StringMap;

/** Main application class
   @author Andre Simon
*/

class HighlightApp {

public:

   HighlightApp(){};
   ~HighlightApp(){};

   /** Start application
     \param argc Number of command line arguments
     \param argv values of command line arguments
     \return EXIT_SUCCESS or EXIT_FAILURE
   */
   int run(int argc, char **argv);

private:

  DataDir dataDir;
  StringMap extensions;
  StringMap scriptShebangs;

  /** print version info*/
  void printVersionInfo();

  /** print error message*/
  void printBadInstallationInfo();

  /** print input and output errors */
  void printIOErrorReport(unsigned int numberErrorFiles, vector<string> & fileList, const string &action);

  /** print installed files
      \param showThemes Print installed themes if true, language definitions otherwise
  */
  bool listInstalledFiles(bool showThemes);

  void printDebugInfo(highlight::LanguageDefinition &lang,
                      const string &langDefPath);

  string getFileSuffix(const string &fileName);

  string guessFileType(const string &suffix, const string &inputFile="");

  int getNumDigits(int i);

  void printProgressBar(int total, int count);
  void printCurrentAction(const string&outfilePath,
                          int total, int count, int countWidth);

  bool readInputFilePaths(vector<string> &fileList, string wildcard,
                          bool recursiveSearch);

  string analyzeShebang(const string& file);
  bool loadMapConfig(const string& name, StringMap* map);

};

#endif
//
// C++ Implementation: platform_fs
//
// Description:
//
//
// Author: André Simon <andre.simon1@gmx.de>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//

#include "platform_fs.h"
#include "./dirstream0.4/dirstream.h"

#include <iostream>

using namespace std;

namespace Platform {

#ifdef _WIN32
	#include <windows.h>

	const char pathSeparator = '\\';
	//const std::string pathSeparatorStr = "\\";

	std::string getAppPath()
	{
		char pathAndName[MAX_PATH], path[MAX_PATH], drive[3];
		GetModuleFileName(NULL, pathAndName, MAX_PATH);
		_splitpath(pathAndName, drive, path, 0, 0);
		return std::string(drive)+path;
	}

#else
       const char pathSeparator = '/';
      // const std::string pathSeparatorStr = "/";

	std::string getAppPath()
	{
		return "";
	}

#endif

bool getDirectoryEntries(vector<string> &fileList,
                string wildcard,
                bool recursiveSearch)
{
  if (!wildcard.empty()) {
	string directory_path;
	string::size_type Pos = wildcard.find_last_of(pathSeparator);
	if (Pos == string::npos) {
		directory_path = ".";
	} else {
		directory_path = wildcard.substr(0, Pos + 1);
		wildcard = wildcard.substr(Pos + 1);
	}

	dirstr::dirstream str( directory_path.c_str(),
                              #ifdef USE_FN_MATCH
                                dirstr::pred_f(FnMatcher(wildcard.c_str(), 0)),
                              #else
                                dirstr::pattern_f(wildcard.c_str()),
                              #endif
                              (recursiveSearch)?dirstr::recursive_yes:dirstr::recursive_no);


	for(string entry; str >> entry;) {
		fileList.push_back(dirstr::full_path(entry));
		//std::cout  << "Entry " <<entry<<"\n";
	}
  }
  return !(fileList.empty());
}

}

//
// C++ Interface: platform_fs
//
// Description:
//
//
// Author: André Simon <andre.simon1@gmx.de>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef PLATFORM_FS__H__INCLUDED
#define PLATFORM_FS__H__INCLUDED

#include <string>
#include <iostream>
#include <vector>

#ifdef USE_FN_MATCH
  #include <fnmatch.h>
#endif

namespace Platform
{
	extern const char pathSeparator;
	//extern const std::string pathSeparatorStr;

	std::string getAppPath();

	/** \param fileList Vector where found entries will be stored
        \param  wildcard Directory path and wildcard
        \param recursiveSearch Test if directory should be searched recursively */
        bool getDirectoryEntries(std::vector<std::string> &fileList,
                           std::string wildcard,
                           bool recursiveSearch=false);

#ifdef USE_FN_MATCH
  struct FnMatcher
  {
    FnMatcher(const char* pattern, int flags)
        : pattern_(pattern)
        , flags_(flags)
    {}
    bool operator()(const std::string& e) const {
	  //  std::cout << "pattern: "<<pattern_<<  "   entry: "<<e.c_str()<< "  Res fn: " <<::fnmatch(pattern_, e.c_str(), FNM_PATHNAME)<< " \n";
        return ! ::fnmatch(pattern_, e.c_str(), flags_);
    }
   private:
    const char* pattern_;
    int flags_;
  };
#endif
}
#endif
/***************************************************************************
                        PreFormatter.cpp  -  description
                             -------------------
    begin                : Mo Jan 03 2005
    copyright            : (C) 2005 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "preformatter.h"

namespace highlight {

PreFormatter::PreFormatter(bool wrap, bool replTabs):
 maxLineLength(80),
 index(0),
 numberSpaces(0),
 wsPrefixLength(string::npos),
 hasMore(false),
 indentAfterOpenBraces(true),
 redefineWsPrefix(false),
 wrapLines(wrap),
 replaceTabs(replTabs)
{
}

PreFormatter::PreFormatter():
 maxLineLength(80),
 index(0),
 numberSpaces(0),
 wsPrefixLength(string::npos),
 hasMore(false),
 indentAfterOpenBraces(true),
 redefineWsPrefix(false),
 wrapLines(false),
 replaceTabs(false)
{
}

PreFormatter::~PreFormatter()
{
}

bool PreFormatter::hasMoreLines(){
  return hasMore;
}

bool PreFormatter::indentCode(){
  return indentAfterOpenBraces;
}

void PreFormatter::setLine(const std::string newLine){

  line=newLine;

  if (replaceTabs && numberSpaces) {
    size_t tabPos=line.find('\t');
    while (tabPos!=string::npos){
       line.replace(tabPos , 1, numberSpaces - (tabPos % numberSpaces) , ' ');
       tabPos = line.find('\t', tabPos+1);
    }
  }

  if (wrapLines){
    wsPrefix.clear();
    index=0;
    wsPrefixLength=string::npos;
    hasMore=true;
    redefineWsPrefix=false;
  }
}

std::string  PreFormatter::getNextLine(){

   if (!wrapLines){
     hasMore = false;
     return line;
   }

   if (!index && line.length() > maxLineLength){ // erster Durchlauf...
      // wenn möglich an öffnender Klammer oder Geichheitszeichen ausrichten
      if (indentAfterOpenBraces){
          wsPrefixLength=line.find_first_of(INDENT_MARKERS);
      }
      // sonst die Einrückung der Originalzeile beibehalten
      if (wsPrefixLength==string::npos || wsPrefixLength-index>maxLineLength){
          wsPrefixLength=line.find_first_not_of(WS_CHARS);
      }
      else {
          // wsPrefix in allen neu umgebrochenen Zeilen durch Spaces ersetzen
          redefineWsPrefix=true;
          //  Position hinter öffnende Klammer springen
          wsPrefixLength=line.find_first_not_of(WS_CHARS,wsPrefixLength+1);
      }

      if (wsPrefixLength!=string::npos){
        index = wsPrefixLength;
        // Falls Anzahl der Whitespaces am beginn der ersten zeile größer
        // als Max. Zeilenlänge, Whitespaces verwerfen
        if (wsPrefixLength>maxLineLength){
          wsPrefixLength=0;
          return string();
        }
        else{
           wsPrefix=line.substr(0, wsPrefixLength);
        }
      }
      // Zeile enthaelt nur Whitespace; verwerfen
      else {
       hasMore= false;
       return string();
      }
   } else {
     if (redefineWsPrefix){
       wsPrefix.clear();
       wsPrefix.append(wsPrefixLength, ' ');
     }
     redefineWsPrefix=false;
   }

   string resultString;

   // Position, ab der rckwaerts nach Umbruchmglichkeit gesucht wird
   unsigned int searchEndPos = maxLineLength - wsPrefixLength;

   // letztes Teilstueck der Zeile ausgeben; Parsen beenden
   if (line.length()-index < searchEndPos) {
     hasMore=false;
     resultString=(index>0) ? wsPrefix + line.substr(index) : line.substr(index);
     return resultString;
   }

   // Umbrechposition suchen
   size_t lbPos = line.find_last_of(LB_CHARS, index+searchEndPos);
   if (lbPos <= index || lbPos == string::npos) {
     // nichts gefunden, hart umbrechen
     lbPos = index + searchEndPos;
   }
   // Einrückung der Originalzeile erhalten
   resultString+=wsPrefix;
   // Neue Zeile erzeugen
   resultString += line.substr(index, lbPos-index+1);

   // Whitespace am neuen Zeilenbeginn ignorieren, ausser beim ersten Durchlauf
   //unsigned int newIndex=StringTools::getNextNonWsPos(line,lbPos+1);
   size_t newIndex=line.find_first_not_of(WS_CHARS, lbPos+1);
   index=(newIndex!=string::npos)?newIndex:line.length();

   hasMore=index!=line.length(); // unnoetigen Leerstring vermeiden

   return resultString;
}

void PreFormatter::setWrappingProperties(unsigned int maxLineLength, bool indentAfterOpenBraces){
    this->maxLineLength = maxLineLength;
    this->indentAfterOpenBraces = indentAfterOpenBraces;
}

void PreFormatter::setNumberSpaces(unsigned int num){
    numberSpaces = num;
}

}
/***************************************************************************
                        PreFormatter.cpp  -  description
                             -------------------
    begin                : Mo Jan 03 2005
    copyright            : (C) 2005 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef PreFormatter_H
#define PreFormatter_H

#define LB_CHARS " \t[](){}-+<>.:,;"
#define WS_CHARS " \n\r\t"
#define INDENT_MARKERS "{(="

#include <string>
#include <iostream>

#include "stringtools.h"

namespace highlight {

/** \brief Class which provides intelligent line wrapping.
* @author Andre Simon
*/

class PreFormatter{
public:
    /** Constructor
    */
    PreFormatter(bool wrap, bool replTabs);

    PreFormatter();

    ~PreFormatter();

    /**
     \return True if current line can be wrapped again
    */
    bool hasMoreLines();

    /**
     Sets new line to be wrapped
     \param newline New line
    */
    void setLine(const std::string  newline);

    /**
     The method will indent function calls and statements
     \return Next line
    */
    std::string  getNextLine();

    /**
     \return True if lines following open braces should be indented
    */
    bool indentCode();

    /**
      \param maxlength max. length of output lines
      \param indentAfterOpenBraces set true if lines should be indented after braces
    */
    void setWrappingProperties(unsigned int maxlength=80, bool indentAfterOpenBraces=true);

    /**
      \param num number of spaces which replace a tab
    */

    void setNumberSpaces(unsigned int num);

    /**
      \return true if preformatting is enabled
    */
    bool isEnabled(){
      return wrapLines || replaceTabs;
    }

private:

    unsigned int maxLineLength;

    std::string line, wsPrefix;
    unsigned int index;
    unsigned int numberSpaces;
    size_t wsPrefixLength;
    bool hasMore, indentAfterOpenBraces;
    bool redefineWsPrefix;
    bool wrapLines, replaceTabs;
};

}

#endif
/***************************************************************************
                          rtfcode.cpp  -  description
                             -------------------
    begin                : Die Jul 9 2002
    copyright            : (C) 2002 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "rtfgenerator.h"

using namespace std;

namespace highlight {

string RtfGenerator::formatStyleAttributes( const ElementStyle & col)
{
  stringstream s;
  s  << "\\red"<< col.getColour().getRTFRedValue()
     << "\\green"<<col.getColour().getRTFGreenValue()
     << "\\blue"<<col.getColour().getRTFBlueValue()
     << ";";

  return s.str();
}

string  RtfGenerator::getOpenTag(int styleNumber,const ElementStyle & elem)
{
  ostringstream s;
  s << "{\\cf"<<(styleNumber+1)<<"{";
  if (elem.isBold()) s << "\\b ";
  if (elem.isItalic()) s << "\\i ";
  if (elem.isUnderline()) s << "\\ul ";
  return  s.str();
}

string  RtfGenerator::getCloseTag(const ElementStyle &elem)
{
  ostringstream s;
  if (elem.isBold()) s << "\\b0 ";
  if (elem.isItalic()) s << "\\i0 ";
  if (elem.isUnderline()) s << "\\ul0 ";
  s << "}}";
  return  s.str();
}

RtfGenerator::RtfGenerator(const string &colourTheme)
    : CodeGenerator( colourTheme)
{
  styleTagOpen.push_back(getOpenTag(STANDARD, docStyle.getDefaultStyle()));
  styleTagOpen.push_back(getOpenTag(STRING, docStyle.getStringStyle()));
  styleTagOpen.push_back(getOpenTag(NUMBER, docStyle.getNumberStyle()));
  styleTagOpen.push_back(getOpenTag(SL_COMMENT, docStyle.getSingleLineCommentStyle()));
  styleTagOpen.push_back(getOpenTag(ML_COMMENT_BEGIN,docStyle.getCommentStyle()));
  styleTagOpen.push_back(getOpenTag(ESC_CHAR, docStyle.getEscapeCharStyle()));
  styleTagOpen.push_back(getOpenTag(DIRECTIVE_LINE, docStyle.getDirectiveStyle()));
  styleTagOpen.push_back(getOpenTag(DIRECTIVE_STRING, docStyle.getDirectiveStringStyle()));
  styleTagOpen.push_back(getOpenTag(LINENUMBER, docStyle.getLineStyle()));
  styleTagOpen.push_back(getOpenTag(SYMBOL, docStyle.getSymbolStyle()));

  styleTagClose.push_back(getCloseTag(docStyle.getDefaultStyle()));
  styleTagClose.push_back(getCloseTag(docStyle.getStringStyle()));
  styleTagClose.push_back(getCloseTag(docStyle.getNumberStyle()));
  styleTagClose.push_back(getCloseTag(docStyle.getSingleLineCommentStyle()));
  styleTagClose.push_back(getCloseTag(docStyle.getCommentStyle()));
  styleTagClose.push_back(getCloseTag(docStyle.getEscapeCharStyle()));
  styleTagClose.push_back(getCloseTag(docStyle.getDirectiveStyle()));
  styleTagClose.push_back(getCloseTag(docStyle.getDirectiveStringStyle()));
  styleTagClose.push_back(getCloseTag(docStyle.getLineStyle()));
  styleTagClose.push_back(getCloseTag(docStyle.getSymbolStyle()));

  newLineTag = "\\par\\pard\n";
  spacer = " ";
}

RtfGenerator::RtfGenerator()
{}
RtfGenerator::~RtfGenerator()
{}

string RtfGenerator::getHeader(const string & title)
{
  return string();
}

void RtfGenerator::printBody()
{
 *out << "{\\rtf1\\ansi\\uc0 \\deff1"
       << "{\\fonttbl{\\f1\\fmodern\\fprq1\\fcharset0 Courier;}}"
       << "{\\colortbl;";

  *out << formatStyleAttributes(docStyle.getDefaultStyle());

  *out << formatStyleAttributes(docStyle.getStringStyle());
  *out << formatStyleAttributes(docStyle.getNumberStyle());
  *out << formatStyleAttributes(docStyle.getSingleLineCommentStyle());

  *out << formatStyleAttributes(docStyle.getCommentStyle());
  *out << formatStyleAttributes(docStyle.getEscapeCharStyle());
  *out << formatStyleAttributes(docStyle.getDirectiveStyle());

  *out << formatStyleAttributes(docStyle.getDirectiveStringStyle());
  *out << formatStyleAttributes(docStyle.getLineStyle());
  *out << formatStyleAttributes(docStyle.getSymbolStyle());

  /* For output formats which can refer to external styles it is more safe
     to use the colour theme's keyword class names, since the language
     definitions (which may change during a batch conversion) do not have to define
     all keyword classes, that are needed to highlight all input files correctly.
     It is ok for RTF to use the language definition's class names, because RTF
     does not refer to external styles.
     We cannot use the theme's class names, because KSIterator returns an
     alphabetically ordered list, which is not good because RTF is dependent
     on the order. We access the keyword style with an ID, which is calculated
     ignoring the alphabetic order.
  */
  vector<string>  keywordClasses = langInfo.getKeywordClasses();
  for (unsigned int i=0;i<keywordClasses.size();i++){
      *out << formatStyleAttributes(docStyle.getKeywordStyle(keywordClasses[i]));
  }

  *out << "}\n{\\info }\\paperw11900\\paperh16820\\margl560\\margr560\\margt840"
       << "\\margb840\\widowctrl\\ftnbj\\aenddoc\\formshade \\fet0\\sectd"
       << "\\linex0\\endnhere\\plain\\f1\\fs20\n\\pard ";
  processRootState();
  *out << "}}"<<endl;
}


string RtfGenerator::getFooter()
{
 return string();
}

/** Gibt RTF-Code der Sonderzeichen zurueck */
string RtfGenerator::maskCharacter(unsigned char c)
{
  switch (c)
    {
    case '}' :
    case '{' :
    case '\\' :
      {
        string m;
        m="\\";
        return m+=c;
      }
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      {
        string m;
        m="{";
        m+=c;
        m+="}";
        return m;
      }
      break;
    case AUML_LC:
      return "\\'e4";
      break;
    case OUML_LC:
      return "\\'f6";
      break;
    case UUML_LC:
      return "\\'fc";
      break;
    case AUML_UC:
      return "\\'c4";
      break;
    case OUML_UC:
      return "\\'d6";
      break;
    case UUML_UC:
      return "\\'dc";
      break;

    case AACUTE_LC:
      return "\\'e1";
      break;
    case EACUTE_LC:
      return "\\'e9";
      break;
    case OACUTE_LC:
      return "\\'f3";
      break;
    case UACUTE_LC:
      return "\\'fa";
      break;

    case AGRAVE_LC:
      return "\\'e0";
      break;
    case EGRAVE_LC:
      return "\\'e8";
      break;
    case OGRAVE_LC:
      return "\\'f2";
      break;
    case UGRAVE_LC:
      return "\\'f9";
      break;

    case AACUTE_UC:
      return "\\'c1";
      break;
    case EACUTE_UC:
      return "\\'c9";
      break;
    case OACUTE_UC:
      return "\\'d3";
      break;
    case UACUTE_UC:
      return "\\'da";
      break;
    case AGRAVE_UC:
      return "\\'c0";
      break;
    case EGRAVE_UC:
      return "\\'c8";
      break;
    case OGRAVE_UC:
      return "\\'d2";
      break;
    case UGRAVE_UC:
      return "\\'d9";
      break;

    case SZLIG:
      return "\\'df";
      break;
   // skip  first byte of multibyte chracters
 /*   #ifndef _WIN32
    case 195:
      return string("");
      break;
#endif*/

    default :
      {
       string m;
       return m += c;
      }
    }
}

string RtfGenerator::getMatchingOpenTag(unsigned int styleID){
 return getOpenTag(KEYWORD+styleID,
        docStyle.getKeywordStyle(langInfo.getKeywordClasses()[styleID]));
}

string RtfGenerator::getMatchingCloseTag(unsigned int styleID){
  return getCloseTag(docStyle.getKeywordStyle(langInfo.getKeywordClasses()[styleID]));
}


}
/***************************************************************************
                          rtfcode.h  -  description
                             -------------------
    begin                : Die Jul 9 2002
    copyright            : (C) 2002 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef RTFGENERATOR_H
#define RTFGENERATOR_H

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

#include "codegenerator.h"
#include "charcodes.h"
#include "version.h"

namespace highlight {

/**
   \brief This class generates RTF.

   It contains information about the resulting document structure (document
   header and footer), the colour system, white space handling and text
   formatting attributes.

* @author Andre Simon
*/

class RtfGenerator : public highlight::CodeGenerator
  {
  public:

   /** Constructor
     \param colourTheme Name of Colour theme to use
    */
    RtfGenerator( const string &colourTheme);
    RtfGenerator();
    ~RtfGenerator();

   /** prints document header
       \param  title Title of the document
    */
    string getHeader(const string & title);

    /** Prints document footer*/
    string getFooter();

    /** Prints document body*/
    void printBody();

  private:

    /** \return escaped character*/
    virtual string maskCharacter(unsigned char );

    /**\return text formatting attributes in RTF format */
    string  formatStyleAttributes( const ElementStyle & col);

    /** gibt RTF-"Tags" zurueck (Farbindex+bold+kursiv)*/
    string getOpenTag(int styleNumber,const ElementStyle &);

    string getCloseTag(const ElementStyle &);

    string getMatchingOpenTag(unsigned int styleID);
    string getMatchingCloseTag(unsigned int styleID);
  };

}
#endif
/***************************************************************************
                          stringtools.cpp  -  description
                             -------------------
    begin                : Mon Dec 10 2001
    copyright            : (C) 2001 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "stringtools.h"

#include <sstream>
#include <iostream>
#include <cctype>

using namespace std;

namespace StringTools
  {
// Make a lowercase copy of s:
// (C) Bruce Eckel, Thinking in C++ Vol 2

string lowerCase(const string& s)
{
  char* buf = new char[s.length()];
  s.copy(buf, s.length());
  for(unsigned int i = 0; i < s.length(); i++)
    buf[i] = tolower(buf[i]);
  string r(buf, s.length());
  delete buf;
  return r;
}

int str2int(string s)
{
  istringstream os(s);
  int intVal;
  os >> intVal;
  return intVal;
}

 bool isAlpha(unsigned char c)
 {
   return (isalpha(c) || c == '_');
 }

string trimRight(const string &value)
 {
  string::size_type where = value.find_last_not_of(" \t\r");

  if (where == string::npos)
   // string has nothing but space
   return string();

  if (where == (value.length() - 1))
   // string has no trailing space, don't copy its contents
   return value;

  return value.substr(0, where + 1);
 }

unsigned char getNextNonWs(const string &line, int index)
{
  unsigned char c;
  do
    {
      c=line[index++];
    }
  while (isspace(c));
  return c;
}

string getParantheseVal(const string &s){
  string::size_type openPos=s.find('(');
  string::size_type closePos=s.rfind(')');
  if (openPos ==string::npos || closePos==string::npos){
    return string();
  }
  return s.substr(openPos+1, closePos-openPos-1);

}

}
/***************************************************************************
                          stringtools.h  -  description
                             -------------------
    begin                : Mon Dec 10 2001
    copyright            : (C) 2001 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef STRINGTOOLS_H
#define STRINGTOOLS_H

#include <string>

using namespace std;

/**\brief Contains methods for string manipulation
  *@author Andre Simon
  */

namespace StringTools
  {

  /** \param s String
      \returns lowercase string  */
  string lowerCase(const string &s);

  /** \param s String
      \returns Integer value */
  int str2int(string s);

  /** \return true if c is alpa or underscore */
  bool isAlpha(unsigned char c);

  /** \param value String
     \return string trimmed on the left side
  */
  string trimRight(const string &value);

  /** \return next character in line starting from index, which is no whitespace*/
  unsigned char getNextNonWs(const string &line, int index=0);

  /** \param s String, containing a opening and a closing paranthesis
       \return value between "(", ")" */
  string getParantheseVal(const string &s);

}

#endif
/***************************************************************************
                          stylecolour.cpp  -  description
                             -------------------
    begin                : Die Nov 5 2002
    copyright            : (C) 2002 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "stylecolour.h"

using std::string;

namespace highlight {

StyleColour::StyleColour(const string & r_hex, const string & g_hex, const string & b_hex)
    : r(r_hex), g(g_hex), b(b_hex)
{}
StyleColour::StyleColour()
    : r("00"), g("00"), b("00")
{}

//Parst PArameter aus style-Datei
StyleColour::StyleColour(const string & styleColourString)
{
  setRGBValues(styleColourString);
}

void StyleColour::setRGBValues(const string & styleColourString){
  //Stringstream zum Einlesen der Tokens:
  istringstream valueStream(styleColourString.c_str());
  valueStream >> r;
  valueStream >> g;
  valueStream >> b;
}

void StyleColour::setRedValue(const string & r_hex)
{
  r = r_hex;
}

void StyleColour::setGreenValue(const string & g_hex)
{
  g = g_hex;
}

void StyleColour::setBlueValue(const string & b_hex)
{
  b = b_hex;
}

string& StyleColour::getHexRedValue()
{
  return r;
}
string& StyleColour::getHexGreenValue()
{
  return g;
}
string& StyleColour::getHexBlueValue()
{
  return b;
}


string StyleColour::getRTFRedValue()
{
  return int2str(hex2dec(r));
}
string StyleColour::getRTFGreenValue()
{
  return int2str(hex2dec(g));
}
string StyleColour::getRTFBlueValue()
{
  return int2str(hex2dec(b));
}


string StyleColour::getLatexRedValue()
{
  return float2str((float)hex2dec(r)/255);
}
string StyleColour::getLatexGreenValue()
{
  return float2str((float)hex2dec(g)/255);
}
string StyleColour::getLatexBlueValue()
{
  return float2str((float)hex2dec(b)/255);
}

// Konvertieren von RGB nach CYM
string StyleColour::getTexRedValue()
{
  return float2str(1-(float)hex2dec(r)/255);
}
string StyleColour::getTexGreenValue()
{
  return float2str(1-(float)hex2dec(g)/255);
}
string StyleColour::getTexBlueValue()
{
  return float2str(1-(float)hex2dec(b)/255);
}


string StyleColour::int2str(const int num)
{
  std::ostringstream outStream;
  outStream << num;

  return outStream.str();
}

string StyleColour::float2str(const double num)
{
  std::ostringstream outStream;
  outStream << ( floor ( num * 100 + .5 ) / 100);

  return outStream.str();
}

int  StyleColour::hex2dec(const string &hexVal)
{

  if (hexVal.length() != 2)
    return 0;

  unsigned int decVal=0, koeff=16;

  for (int i=0; i<2;i++ )
    {
      if ((hexVal[i] >= '0')&& (hexVal[i]<= '9' ))
        {
          decVal += (koeff * (hexVal[i]-'0'));

        }
      if ((hexVal[i] >= 'a')&& (hexVal[i]<= 'f' ))
        {
          decVal +=( koeff * (hexVal[i]-87));
        }
      if ((hexVal[i] >= 'A')&& (hexVal[i]<= 'F' ))
        {
          decVal += (koeff * (hexVal[i]-55));
        }
      koeff=1;
    }
  return decVal;
}

}

/***************************************************************************
                          stylecolour.h  -  description
                             -------------------
    begin                : Die Nov 5 2002
    copyright            : (C) 2002 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef STYLECOLOUR_H
#define STYLECOLOUR_H

#include <string>
#include <sstream>
#include <fstream>
#include <cmath>
#include <sstream>

using namespace std;

namespace highlight {

/**\brief Stores colours and returns red, green and blue values in different formats
* @author Andre Simon
 */

class StyleColour
  {
  public:
    /** Constructor
        \param r_hex Red value in hex notation
        \param g_hex Blue value in hex notation
        \param b_hex Green value in hex notation
    */
    StyleColour(const string & r_hex, const string & g_hex, const string & b_hex);

    /** Constructor
        \param styleColourString String with rgb values
    */
    StyleColour(const string & styleColourString);

    StyleColour();
    ~StyleColour(){};

   /** Sets red, green and blue values
     \param styleColourString String containing colour attributes
   */
   void setRGBValues(const string & styleColourString);

    /** Sets red value
        \param r_hex New red value */
    void setRedValue(const string & r_hex);

    /** Sets green value
        \param g_hex New green value */
    void setGreenValue(const string & g_hex);

    /** Sets blue value
        \param b_hex New blue value */
    void setBlueValue(const string & b_hex);

    /** \return Red value in hex format */
    string& getHexRedValue();
    /** \return Green value in hex format */
    string& getHexGreenValue();
    /** \return Blue value in hex format */
    string& getHexBlueValue();

    /** \return Red value in latex format */
    string getLatexRedValue();
    /** \return Green value in latex format */
    string getLatexGreenValue();
    /** \return Blue value in latex format */
    string getLatexBlueValue();

    /** \return Red value in tex format */
    string getTexRedValue();
    /** \return Green value in tex format */
    string getTexGreenValue();
    /** \return Blue value in tex format */
    string getTexBlueValue();

    /** \return Red value in RTF format */
    string getRTFRedValue();
    /** \return Green value in RTF format */
    string getRTFGreenValue();
    /** \return Blue value in RTF format */
    string getRTFBlueValue();

  private:
    string r, g, b;
    string int2str(int);
    string float2str(double);
    int hex2dec(const string &hexVal);
  };

}

#endif
/***************************************************************************
                          TexGenerator.cpp  -  description
                             -------------------
    begin                : Mit Jul 24 2002
    copyright            : (C) 2002 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "texgenerator.h"

namespace highlight {

TexGenerator::TexGenerator(const string &colourTheme):
  CodeGenerator( colourTheme)
{
  styleTagOpen.push_back( "{\\hlstd ");
  styleTagOpen.push_back( "{\\hlstr ");
  styleTagOpen.push_back( "{\\hlnum ");
  styleTagOpen.push_back( "{\\hlslc ");
  styleTagOpen.push_back( "{\\hlcom ");
  styleTagOpen.push_back( "{\\hlesc ");
  styleTagOpen.push_back( "{\\hldir ");
  styleTagOpen.push_back( "{\\hldstr ");
  styleTagOpen.push_back( "{\\hlline ");
  styleTagOpen.push_back( "{\\hlsym ");
  for (int i=0;i<NUMBER_BUILTIN_STYLES; i++)    {
    styleTagClose.push_back( "}");
  }

  /*This makes TeX to use every par it encounters (the \\leavevmode has
  no effect when TeX is in horizontal mode and when TeX is in vertical
  mode, it switches it to horizontal mode).*/
  newLineTag="\\leavevmode\\par\n";

  spacer = "\\ ";
  maskWs=true;
  excludeWs=true;
  maskWsBegin = "{\\hlstd";
  maskWsEnd = "}";
  styleCommentOpen="%";
}

TexGenerator::TexGenerator()
{}
TexGenerator::~TexGenerator()
{}

string TexGenerator::formatStyleAttributes(const string & elemName,const ElementStyle & elem)
{
  ostringstream s;
  s << "\\def\\hl"
    << elemName
    << "{";
  if (elem.isBold())
    s << "\\bf";
  if (elem.isItalic())
    s << "\\it";
  s  <<  "\\textColor{"
     << (elem.getColour().getTexRedValue())<<" "
     << (elem.getColour().getTexGreenValue())<<" "
     << (elem.getColour().getTexBlueValue())<<" 0.0}}\n";
  return  s.str();
}

string TexGenerator::getHeader(const string & title)
{
  ostringstream os;

  if (langInfo.getSyntaxHighlight()) {
    if (includeStyleDef) {
       os << "\n"<<getStyleDefinition();
       os << CodeGenerator::readUserStyleDef();
    }  else  {
       os << "\\input "
          << getStyleOutputPath()
          << "\n\n";
    }
  }

  return os.str();
}

void TexGenerator::printBody()
{
    *out << "{\n\\tt\n";

  processRootState();
  *out << "}\n";
}

string TexGenerator::getFooter()
{
  ostringstream os;
  os << "\\bye\n"
     << "% TeX generated by Highlight "
     << HIGHLIGHT_VERSION
     << ", "
     << HIGHLIGHT_URL
     << endl;
  return os.str();
}

string TexGenerator:: maskCharacter(unsigned char c)
{
  switch (c)
    {
    case '{':
    case '}':
      {
        string m;
        m = "$\\";
        m += c;
        m += "$";
        return m;
      }
      break;
    case '^':
      return "{\\bf\\^{}}";
      break;
    case '_':
      return "\\_{}";
      break;
    case '&':
    case '$':
    case '#':
    case '%':
      {
        string m;
        m = "\\";
        m += c;
        return m;
      }
      break;
    case '\\':
      return "$\\backslash$";
      break;
    case ' ':
      return spacer;
      break;
    case '+':
    case '-':
    case '<':
    case '>':
    case '=':
      {
        string m;
        m = "$\\mathord{";
        m += c;
        m += "}$";
        return m;
      }
      break;
    case AUML_LC:
      return "\\\"a";
      break;
    case OUML_LC:
      return "\\\"o";
      break;
    case UUML_LC:
      return "\\\"u";
      break;
    case AUML_UC:
      return "\\\"A";
      break;
    case OUML_UC:
      return "\\\"O";
      break;
    case UUML_UC:
      return "\\\"U";
      break;
    case AACUTE_LC:
      return "\\'a";
      break;
    case EACUTE_LC:
      return "\\'e";
      break;
    case OACUTE_LC:
      return "\\'o";
      break;
    case UACUTE_LC:
      return "\\'u";
      break;
    case AGRAVE_LC:
      return "\\`a";
      break;
    case EGRAVE_LC:
      return "\\`e";
      break;
    case OGRAVE_LC:
      return "\\`o";
      break;
    case UGRAVE_LC:
      return "\\`u";
      break;
    case AACUTE_UC:
      return "\\'A";
      break;
    case EACUTE_UC:
      return "\\'E";
      break;
    case OACUTE_UC:
      return "\\'O";
      break;
    case UACUTE_UC:
      return "\\'U";
      break;
    case AGRAVE_UC:
      return "\\`A";
      break;
    case EGRAVE_UC:
      return "\\`E";
      break;
    case UGRAVE_UC:
      return "\\`O";
      break;
    case OGRAVE_UC:
      return "\\`U";
      break;
    case SZLIG:
      return "\\ss ";
      break;
 /*    #ifndef _WIN32
 // skip  first byte of multibyte chracters
    case 195:
      return string("");
      break;
#endif*/

    default :
      string m;
      return m += c;
    }
}

string TexGenerator::getMatchingOpenTag(unsigned int styleID){
  return "{\\hl"+langInfo.getKeywordClasses()[styleID]+" ";
 }

string TexGenerator::getMatchingCloseTag(unsigned int styleID){
  return "}";
}


string TexGenerator::getStyleDefinition()
{
    if (styleDefinitionCache.empty()){
        ostringstream os;
        os << formatStyleAttributes("std", docStyle.getDefaultStyle());
        os << formatStyleAttributes("num", docStyle.getNumberStyle());
        os << formatStyleAttributes("esc", docStyle.getEscapeCharStyle());
        os << formatStyleAttributes("str", docStyle.getStringStyle());
        os << formatStyleAttributes("dstr", docStyle.getDirectiveStringStyle());
        os << formatStyleAttributes("slc", docStyle.getSingleLineCommentStyle());
        os << formatStyleAttributes("com", docStyle.getCommentStyle());
        os << formatStyleAttributes("dir", docStyle.getDirectiveStyle());
        os << formatStyleAttributes("line", docStyle.getLineStyle());
        os << formatStyleAttributes("sym", docStyle.getSymbolStyle());

        KeywordStyles styles = docStyle.getKeywordStyles();
        for (KSIterator it=styles.begin(); it!=styles.end(); it++){
            os << formatStyleAttributes(it->first, *(it->second));
        }

        os <<  "% The special option is not supported by all dvi drivers\n"
           << "\\special{background rgb "
           << docStyle.getBgColour().getLatexRedValue() << " "
           << docStyle.getBgColour().getLatexGreenValue() << " "
           << docStyle.getBgColour().getLatexBlueValue() << "}";
        os << "\n\\nopagenumbers\n"
           << "\\input colordvi\n";
        styleDefinitionCache=os.str();
    }
    return styleDefinitionCache;
}


}
/***************************************************************************
                          texcode.h  -  description
                             -------------------
    begin                : Mit Jul 24 2002
    copyright            : (C) 2002 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef TEXGENERATOR_H
#define TEXGENERATOR_H

#include <string>
#include <iostream>
#include <sstream>

#include "charcodes.h"
#include "version.h"
#include "codegenerator.h"


namespace highlight {

/**
   \brief This class generates TeX.

   It contains information about the resulting document structure (document
   header and footer), the colour system, white space handling and text
   formatting attributes.

* @author Andre Simon
*/

class TexGenerator : public highlight::CodeGenerator
  {
  public:

   /** Constructor
     \param colourTheme Name of Colour theme to use
    */
    TexGenerator(const string &colourTheme);
    TexGenerator();
    ~TexGenerator();

   /** prints document header
       \param  title Title of the document
    */
    string getHeader(const string & title);

    /** Prints document footer*/
    string getFooter();

    /** Prints document body*/
    void printBody();

  private:

      string styleDefinitionCache;

      string getStyleDefinition();

    /** \return escaped character*/
    virtual string maskCharacter(unsigned char );

    /**\return text formatting attributes in RTF format */
    string formatStyleAttributes(const string & elemName, const ElementStyle & elem);

    string getMatchingOpenTag(unsigned int styleID);
    string getMatchingCloseTag(unsigned int styleID);

  };

}

#endif
/***************************************************************************
                          version.h  -  description
                             -------------------
    begin                : Mon March 3 2003
    copyright            : (C) 2003 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef VERSION_H
#define VERSION_H

#define HIGHLIGHT_VERSION "2.2-10"

#define HIGHLIGHT_URL "http://www.andre-simon.de/"
#define HIGHLIGHT_EMAIL "andre.simon1@gmx.de"

#endif
/***************************************************************************
                          htmlcode.cpp  -  description
                             -------------------
    begin                : Wed Nov 28 2001
    copyright            : (C) 2001 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "xhtmlgenerator.h"

using namespace std;

namespace highlight {

XHtmlGenerator::XHtmlGenerator(void)
{}

XHtmlGenerator::XHtmlGenerator (
  const string &cssStyle,
  const string &enc,
  bool omitEnc,
  bool withAnchors)
    : HtmlGenerator(cssStyle, enc, omitEnc, withAnchors)
{
   fileSuffix=".xhtml";
   brTag="<br />";
   hrTag="<hr />";
   idAttr="id";

  HTML_FOOTER=
  "\n</body>\n</html>\n<!--XHTML generated by highlight "
  HIGHLIGHT_VERSION
  ", "
  HIGHLIGHT_URL
  "-->\n";
}

string XHtmlGenerator::getHeaderStart(const string &title){
    ostringstream header;
    header << "<?xml version=\"1.0\"";
    if (!omitEncoding) {
      header << " encoding=\"" << encoding << "\"";
    }
    header << "?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\""
           << "  \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n"
           << "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
           << "<head>\n<title>" << title << "</title>\n";

    return header.str();
}


string XHtmlGenerator::getHeader(const string &title)
{
  ostringstream osPart1;
  osPart1 << getHeaderStart((title.empty())?"Source file":title );

  if (langInfo.getSyntaxHighlight())
    {
      if (includeStyleDef)    //CSS-Definition in HTML-<head> einfuegen
        {
          osPart1 << "<style type=\"text/css\">\n";
          osPart1 << "<![CDATA[\n";
          osPart1 << getStyleDefinition();
          osPart1 << CodeGenerator::readUserStyleDef();
          osPart1 << "]]>\n";
          osPart1 << "</style>\n";
        }
      else  //Referenz auf CSS-Datei einfuegen
        {
          osPart1 << "<link rel=\"stylesheet\" type=\"text/css\" href=\""
                  << getStyleOutputPath()
                  << "\""
                  << "/"
                  << ">\n";
        }
    }
    osPart1 << "</head>\n<body class=\"hl\">\n<pre class=\"hl\">";

  return osPart1.str();
}

}
/***************************************************************************
                         xhtmlgenerator.h  -  description
                             -------------------
    begin                : Mo Jun 21 2004
    copyright            : (C) 2004 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


#ifndef XHTMLGENERATOR_H
#define XHTMLGENERATOR_H

#include "htmlgenerator.h"

namespace highlight {

/**
   \brief This class generates XHTML.

   It contains information about the resulting document structure (document
   header and footer), the colour system, white space handling and text
   formatting attributes.

* @author Andre Simon
*/


class XHtmlGenerator : public highlight::HtmlGenerator
  {
  public:

    /** Constructor
     \param colourTheme Name of Colour theme to use
     \param enc encoding name
     \param omitEnc switch to omit encoding information
     \param withAnchors Test if HTML anchors should be attached to line numbers
    */
    XHtmlGenerator(const string &colourTheme,
                   const string &enc,
                   bool omitEnc=false,
                   bool withAnchors = false);

    XHtmlGenerator();

    /** Destructor*/
    virtual ~XHtmlGenerator() {};

  private:

    /** prints document header
       \param  title Title of the document
    */
    string getHeader(const string &title);

    string getHeaderStart(const string &title);

  };

}

#endif
/***************************************************************************
                          xmlcode.cpp  -  description
                             -------------------
    begin                : Do 20.01.2005
    copyright            : (C) 2005 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "xmlgenerator.h"

using namespace std;
namespace highlight {

    XmlGenerator::XmlGenerator(const string &colourTheme,const string &enc, bool omitEnc)
    : CodeGenerator(colourTheme),
    encoding(enc), omitEncoding(omitEnc)
{
    styleTagOpen.push_back(getOpenTag("def"));
    styleTagOpen.push_back(getOpenTag("str"));
    styleTagOpen.push_back(getOpenTag("num"));
    styleTagOpen.push_back(getOpenTag("slc"));
    styleTagOpen.push_back(getOpenTag("com"));
    styleTagOpen.push_back(getOpenTag("esc"));
    styleTagOpen.push_back(getOpenTag("dir"));
    styleTagOpen.push_back(getOpenTag("dstr"));
    styleTagOpen.push_back(getOpenTag("line"));
    styleTagOpen.push_back(getOpenTag("sym"));

    styleTagClose.push_back(getCloseTag("def"));
    styleTagClose.push_back(getCloseTag("str"));
    styleTagClose.push_back(getCloseTag("num"));
    styleTagClose.push_back(getCloseTag("slc"));
    styleTagClose.push_back(getCloseTag("com"));
    styleTagClose.push_back(getCloseTag("esc"));
    styleTagClose.push_back(getCloseTag("dir"));
    styleTagClose.push_back(getCloseTag("dstr"));
    styleTagClose.push_back(getCloseTag("line"));
    styleTagClose.push_back(getCloseTag("sym"));

    spacer = " ";
    newLineTag = "<br />\n";
}

string XmlGenerator::getStyleDefinition()
{
    if (styleDefinitionCache.empty()) {
        ostringstream os;
        os << "\n<style>\n"
           << "\t<bgcolor value=\""
           << (docStyle.getBgColour().getHexRedValue())
           << (docStyle.getBgColour().getHexGreenValue())
           << (docStyle.getBgColour().getHexBlueValue())
           << "\" />\n"
           << "\t<font size=\""
           << docStyle.getFontSize()
           << "\" family=\"Courier\" />\n";

        os << formatStyleAttributes("def", docStyle.getDefaultStyle())
           << formatStyleAttributes("num", docStyle.getNumberStyle())
           << formatStyleAttributes("esc", docStyle.getEscapeCharStyle())
           << formatStyleAttributes("str", docStyle.getStringStyle())
           << formatStyleAttributes("dstr", docStyle.getDirectiveStringStyle())
           << formatStyleAttributes("slc", docStyle.getSingleLineCommentStyle())
           << formatStyleAttributes("com", docStyle.getCommentStyle())
           << formatStyleAttributes("dir", docStyle.getDirectiveStyle())
           << formatStyleAttributes("sym", docStyle.getSymbolStyle())
           << formatStyleAttributes("line", docStyle.getLineStyle());

        KeywordStyles styles = docStyle.getKeywordStyles();
        for (KSIterator it=styles.begin(); it!=styles.end(); it++){
            os << formatStyleAttributes(it->first, *(it->second));
        }
        os << "</style>\n";
        styleDefinitionCache=os.str();
    }
    return styleDefinitionCache;
}


string XmlGenerator::formatStyleAttributes(const string & elemName,
                                           const ElementStyle & elem)
{
    ostringstream s;
    s  << "\t<class name=\""
       << elemName
       <<"\" color=\""
       << (elem.getColour().getHexRedValue())
       << (elem.getColour().getHexGreenValue())
       << (elem.getColour().getHexBlueValue()  )
       << "\" bold=\""
       << ( elem.isBold() ? "true" :"false" )
       << "\" italic=\""
       << ( elem.isItalic() ? "true" :"false" )
       << "\" underline=\""
       << ( elem.isUnderline() ? "true" :"false" )
       << "\" />\n" ;
    return  s.str();
}


XmlGenerator::XmlGenerator()
{}
XmlGenerator::~XmlGenerator()
{}

string  XmlGenerator::getOpenTag(const string& styleName ){
    return "<"+styleName+">";
}

string  XmlGenerator::getCloseTag(const string& styleName ){
    return "</"+styleName+">";
}

string XmlGenerator::getHeader(const string & title)
{
    ostringstream header;
    header << "<?xml version=\"1.0\"";
    if (!omitEncoding) {
        header << " encoding=\"" << encoding << "\"";
    }
    header << "?>\n<document>" << getStyleDefinition();
    return header.str();
}

void XmlGenerator::printBody()
{
  *out << "<source>\n";
  processRootState();
  *out << "</source>\n";
}


string XmlGenerator::getFooter()
{
  ostringstream os;
  os <<"</document>\n";
  os<< "<!-- XML generated by Highlight "
     << HIGHLIGHT_VERSION
     << ", "
     << HIGHLIGHT_URL
     <<" -->\n";
  return os.str();
}

string XmlGenerator::maskCharacter(unsigned  char c)
{
  switch (c)
    {
      case '<' :
      return "&lt;";
      break;
    case '>' :
      return "&gt;";
      break;
    case '&' :
      return "&amp;";
      break;
    case '\"' :
      return "&quot;";
      break;

// skip  first byte of multibyte chracters
  /*  #ifndef _WIN32
    case 195:
      return string("");
      break;
#endif*/

    default:
      string m;
      m += c;
      return m;
    }
}

/*string XmlGenerator::getNewLine(){
  string nlStr;
  if (currentState!=_UNKNOWN){
      nlStr+=styleTagClose[getStyleID(currentState, currentKeywordClass)];
  }
  nlStr += newLineTag;
  if (currentState!=_UNKNOWN){
      nlStr+=styleTagOpen[getStyleID(currentState, currentKeywordClass)];
  }
  return nlStr;
}
*/
string XmlGenerator::getMatchingOpenTag(unsigned int styleID){
    return getOpenTag(langInfo.getKeywordClasses()[styleID]);
}

string XmlGenerator::getMatchingCloseTag(unsigned int styleID){
    return getCloseTag(langInfo.getKeywordClasses()[styleID]);
}

}
/***************************************************************************
                          xmlcode.h  -  description
                             -------------------
    begin                : Do 20.01.2005
    copyright            : (C) 2005 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef XMLGENERATOR_H
#define XMLGENERATOR_H

#include <string>
#include <sstream>
#include <iostream>

#include "codegenerator.h"
#include "version.h"

namespace highlight {

/**
   \brief This class generates XML.

   It contains information about the resulting document structure (document
   header and footer), the colour system, white space handling and text
   formatting attributes.

* @author Andre Simon
*/

class XmlGenerator : public highlight::CodeGenerator
  {
  public:

   /** Constructor
     \param colourTheme Name of Colour theme to use
     \param enc encoding name
     \param omitEnc switch to omit encoding information
    */
      XmlGenerator( const string &colourTheme,const string &enc, bool omitEnc=false);

   XmlGenerator();

    ~XmlGenerator();

   /** prints document header
       \param  title Title of the document
    */
    string getHeader(const string & title);

    /** Prints document footer*/
    string getFooter();

    /** Prints document body*/
    void printBody();

  private:

    string styleDefinitionCache, encoding;

    bool omitEncoding;

    string getStyleDefinition();

    string formatStyleAttributes(const string &, const ElementStyle &);

    /** \return escaped character*/
    virtual string maskCharacter(unsigned char );


//    string getNewLine();

    string getOpenTag(const string& );
    string getCloseTag(const string& );

    string getMatchingOpenTag(unsigned int styleID);
    string getMatchingCloseTag(unsigned int styleID);
  };

}

#endif
/***************************************************************************
                          xslfocode.cpp  -  description
                             -------------------
    begin                : Do 11.12.2003
    copyright            : (C) 2003 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "xslfogenerator.h"

using namespace std;
namespace highlight {

XslFoGenerator::XslFoGenerator(const string &colourTheme,
                               const string &enc,
                               bool omitEnc,
                               bool fopCompatible)
    : CodeGenerator(colourTheme),
     encoding(enc),
     fopOutput(fopCompatible),
     omitEncoding(omitEnc)
{
  styleTagOpen.push_back( getOpenTag(docStyle.getDefaultStyle()));
  styleTagOpen.push_back( getOpenTag(docStyle.getStringStyle()));
  styleTagOpen.push_back( getOpenTag(docStyle.getNumberStyle()));
  styleTagOpen.push_back( getOpenTag(docStyle.getSingleLineCommentStyle()));
  styleTagOpen.push_back( getOpenTag(docStyle.getCommentStyle()));
  styleTagOpen.push_back( getOpenTag(docStyle.getEscapeCharStyle()));
  styleTagOpen.push_back( getOpenTag(docStyle.getDirectiveStyle()));
  styleTagOpen.push_back( getOpenTag(docStyle.getDirectiveStringStyle()));
  styleTagOpen.push_back( getOpenTag(docStyle.getLineStyle()));
  styleTagOpen.push_back( getOpenTag(docStyle.getSymbolStyle()));
  snl << "  <fo:block font-size=\""
      << docStyle.getFontSize()
      << "pt\" font-family=\"Courier\" white-space-collapse=\"false\" "
      << "wrap-option=\"wrap\" line-height=\"12pt\" background-color=\"#"
      << (docStyle.getBgColour().getHexRedValue())
      << (docStyle.getBgColour().getHexGreenValue())
      << (docStyle.getBgColour().getHexBlueValue())
      << "\">";

 for (int i=0;i<NUMBER_BUILTIN_STYLES; i++)
  {
    styleTagClose.push_back( "</fo:inline>");
  }
  if (fopOutput)
    newLineTag ="</fo:block>\n<fo:block>";
  else
    newLineTag ="</fo:block>\n"+ snl.str();

  spacer = " ";
}

XslFoGenerator::XslFoGenerator()
{}
XslFoGenerator::~XslFoGenerator()
{}

string  XslFoGenerator::getOpenTag(const ElementStyle &elem)
{
  ostringstream s;
  s  << "<fo:inline color=\"#"
     << (elem.getColour().getHexRedValue())
     << (elem.getColour().getHexGreenValue())
     << (elem.getColour().getHexBlueValue())
     << "\"";
   s << ( elem.isBold() ?" font-weight=\"bold\"" :"" )
     << ( elem.isItalic() ?" font-style=\"italic\"" :"" )
     << ( elem.isUnderline() ?" text-decoration=\"underline\"" :"" );
   s << ">";
  return  s.str();
}

string XslFoGenerator::getHeader(const string & title)
{
   ostringstream os;
   os << "<?xml version=\"1.0\"";
   if (!omitEncoding) {
       os << " encoding=\"" << encoding << "\"";
   }
   os << "?>\n<fo:root xmlns:fo=\"http://www.w3.org/1999/XSL/Format\">\n"
      << "<fo:layout-master-set>\n"
      << "<fo:simple-page-master master-name=\"DINA4\"\n"
      << "    page-height=\"29.7cm\"\n"
      << "    page-width=\"21cm\"\n"
      << "    margin-top=\"1cm\"\n"
      << "    margin-bottom=\"2cm\"\n"
      << "    margin-left=\"2.5cm\"\n"
      << "    margin-right=\"2.5cm\">\n"
      << "<fo:region-body />\n"
      << "</fo:simple-page-master>\n"
      << "<fo:page-sequence-master master-name=\"basic\">\n"
      << "<fo:repeatable-page-master-alternatives>\n"
      << "<fo:conditional-page-master-reference master-reference=\"DINA4\" />\n"
      << "</fo:repeatable-page-master-alternatives>\n"
      << "</fo:page-sequence-master>\n"
      << "</fo:layout-master-set>\n\n"
      << "<fo:page-sequence master-reference=\"basic\">\n"
      << " <fo:flow flow-name=\"xsl-region-body\">\n";
   if (fopOutput)
     os << snl.str()<< "<fo:block>";
   else
     os << snl.str();

  return  os.str();
}

/** gibt RTF-Text aus */
void XslFoGenerator::printBody()
{
  processRootState();
}


string XslFoGenerator::getFooter()
{
  ostringstream os;
  if (fopOutput)
    os <<"\n</fo:block>";
  os <<"\n</fo:block>\n </fo:flow>\n</fo:page-sequence>\n</fo:root>"<<endl
     << "<!-- XSL-FO generated by Highlight "
     << HIGHLIGHT_VERSION
     << ", "
     << HIGHLIGHT_URL
     <<" -->\n";
  return os.str();
}

/** Gibt RTF-Code der Sonderzeichen zurueck */
string XslFoGenerator::maskCharacter(unsigned  char c)
{
  switch (c)
    {
      case '<' :
      return "&lt;";
      break;
    case '>' :
      return "&gt;";
      break;
    case '&' :
      return "&amp;";
      break;
    case '\"' :
      return "&quot;";
      break;

// skip  first byte of multibyte chracters
    /*#ifndef _WIN32
    case 195:
      return string("");
      break;
#endif*/

    default:
      string m;
      m += c;
      return m;
    }
}

/*string XslFoGenerator::getNewLine(){
  string nlStr;

  if (currentState!=_UNKNOWN){
      nlStr+=styleTagClose[getStyleID(currentState, currentKeywordClass)];
}
  nlStr += newLineTag;
  if (currentState!=_UNKNOWN){
      nlStr+=styleTagOpen[getStyleID(currentState, currentKeywordClass)];
}
  return nlStr;
}*/

string XslFoGenerator::getMatchingOpenTag(unsigned int styleID){
 return getOpenTag(docStyle.getKeywordStyle(langInfo.getKeywordClasses()[styleID]));
}

string XslFoGenerator::getMatchingCloseTag(unsigned int styleID){
  return "</fo:inline>";
}

}
/***************************************************************************
                          xslfocode.h  -  description
                             -------------------
    begin                : Do 11.12.2003
    copyright            : (C) 2003 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef XSLFOGENERATOR_H
#define XSLFOGENERATOR_H


#include <string>
#include <sstream>
#include <iostream>
#include <fstream>

#include "codegenerator.h"
#include "version.h"

namespace highlight {

/**
   \brief This class generates XSL-FO.

   It contains information about the resulting document structure (document
   header and footer), the colour system, white space handling and text
   formatting attributes.

* @author Andre Simon
*/

class XslFoGenerator : public highlight::CodeGenerator
  {
  public:

   /** Constructor
     \param colourTheme Name of Colour theme to use
     \param enc encoding name
     \param omitEnc switch to omit encoding information
     \param fopCompatible Test if output should be compatible with Apache FOP 0.20.5
    */
    XslFoGenerator( const string &colourTheme,
                    const string &enc,
                    bool omitEnc=false,
                    bool fopCompatible=false);

    XslFoGenerator();

    ~XslFoGenerator();

   /** prints document header
       \param  title Title of the document
    */
    string getHeader(const string & title);

    /** Prints document footer*/
    string getFooter();

    /** Prints document body*/
    void printBody();

  private:
    ostringstream snl;

    string styleDefinition, encoding;
    bool fopOutput, omitEncoding;

    /** \return escaped character*/
    virtual string maskCharacter(unsigned char );

    string getOpenTag(const ElementStyle &);

  //  string getNewLine();

    string getMatchingOpenTag(unsigned int styleID);
    string getMatchingCloseTag(unsigned int styleID);
  };

}

#endif
