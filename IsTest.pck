'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 16 June 2012 at 7:56:56 pm'!
'Description Notes:
noteCompilationOf:meta: not sure how to avoid multiple firings for subclasses or if it''s worth worrying about.

"Run timing and memory tests (results to Transcript)"
LetsGetDownToIsness tests.'!
!classDefinition: #IsTests category: #IsTest!
TestCase subclass: #IsTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'IsTests class' category: #IsTest!
IsTests class
	instanceVariableNames: ''!

!classDefinition: #LetsGetDownToIsness category: #IsTest!
Object subclass: #LetsGetDownToIsness
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'LetsGetDownToIsness class' category: #IsTest!
LetsGetDownToIsness class
	instanceVariableNames: ''!

!classDefinition: #ObjectRoot category: #IsTest!
Object subclass: #ObjectRoot
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectRoot class' category: #IsTest!
ObjectRoot class
	instanceVariableNames: ''!

!classDefinition: #ObjectRootNewIs category: #IsTest!
ObjectRoot subclass: #ObjectRootNewIs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectRootNewIs class' category: #IsTest!
ObjectRootNewIs class
	instanceVariableNames: 'protocols'!

!classDefinition: #ObjectMultiNewIs category: #IsTest!
ObjectRootNewIs subclass: #ObjectMultiNewIs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectMultiNewIs class' category: #IsTest!
ObjectMultiNewIs class
	instanceVariableNames: ''!

!classDefinition: #ObjectMultiSubNewIs category: #IsTest!
ObjectMultiNewIs subclass: #ObjectMultiSubNewIs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectMultiSubNewIs class' category: #IsTest!
ObjectMultiSubNewIs class
	instanceVariableNames: ''!

!classDefinition: #ObjectNoneNewIs category: #IsTest!
ObjectRootNewIs subclass: #ObjectNoneNewIs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectNoneNewIs class' category: #IsTest!
ObjectNoneNewIs class
	instanceVariableNames: ''!

!classDefinition: #ObjectRootOldIs category: #IsTest!
ObjectRoot subclass: #ObjectRootOldIs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectRootOldIs class' category: #IsTest!
ObjectRootOldIs class
	instanceVariableNames: ''!

!classDefinition: #ObjectMultiOldIs category: #IsTest!
ObjectRootOldIs subclass: #ObjectMultiOldIs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectMultiOldIs class' category: #IsTest!
ObjectMultiOldIs class
	instanceVariableNames: ''!

!classDefinition: #ObjectMultiSubOldIs category: #IsTest!
ObjectMultiOldIs subclass: #ObjectMultiSubOldIs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectMultiSubOldIs class' category: #IsTest!
ObjectMultiSubOldIs class
	instanceVariableNames: ''!

!classDefinition: #ObjectNoneOldIs category: #IsTest!
ObjectRootOldIs subclass: #ObjectNoneOldIs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectNoneOldIs class' category: #IsTest!
ObjectNoneOldIs class
	instanceVariableNames: ''!

!classDefinition: #ObjectSingleNewIs category: #IsTest!
ObjectRootNewIs subclass: #ObjectSingleNewIs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectSingleNewIs class' category: #IsTest!
ObjectSingleNewIs class
	instanceVariableNames: ''!

!classDefinition: #ObjectSingleOldIs category: #IsTest!
ObjectRootOldIs subclass: #ObjectSingleOldIs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'IsTest'!
!classDefinition: 'ObjectSingleOldIs class' category: #IsTest!
ObjectSingleOldIs class
	instanceVariableNames: ''!


!IsTests methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 14:09'!
testMulti
	| ni oi |
	ni := ObjectMultiNewIs new.
	self assert: (ni is: #is).
	self deny: (ni is: #isNess).
	self assert: (ni is: #isNess1).
	self assert: (ni is: #isNess2).
	self assert: (ni is: #isNess3).
	self deny: (ni is: #isNessSub).
	self deny: (ni is: #noWay).
	oi := ObjectMultiOldIs new.
	self assert: (oi is: #is).
	self deny: (oi is: #isNess).
	self assert: (oi is: #isNess1).
	self assert: (oi is: #isNess2).
	self assert: (oi is: #isNess3).
	self deny: (oi is: #isNessSub).
	self deny: (oi is: #noWay).! !

!IsTests methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 14:14'!
testMultiSub
	| ni oi |
	ni := ObjectMultiSubNewIs new.
	self assert: (ni is: #is).
	self deny: (ni is: #isNess).
	self assert: (ni is: #isNess1).
	self assert: (ni is: #isNess2).
	self assert: (ni is: #isNess3).
	self assert: (ni is: #isNessSub).
	self deny: (ni is: #noWay).
	oi := ObjectMultiSubOldIs new.
	self assert: (oi is: #is).
	self deny: (oi is: #isNess).
	self assert: (oi is: #isNess1).
	self assert: (oi is: #isNess2).
	self assert: (oi is: #isNess3).
	self assert: (oi is: #isNessSub).
	self deny: (oi is: #noWay).! !

!IsTests methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 14:05'!
testNone
	| ni oi |
	ni := ObjectNoneNewIs new.
	self assert: (ni is: #is).
	self deny: (ni is: #isNess).
	self deny: (ni is: #isNess1).
	self deny: (ni is: #isNess2).
	self deny: (ni is: #isNess3).
	self deny: (ni is: #isNessSub).
	self deny: (ni is: #noWay).
	oi := ObjectNoneOldIs new.
	self assert: (oi is: #is).
	self deny: (oi is: #isNess).
	self deny: (oi is: #isNess1).
	self deny: (oi is: #isNess2).
	self deny: (oi is: #isNess3).
	self deny: (oi is: #isNessSub).
	self deny: (oi is: #noWay).! !

!IsTests methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 14:06'!
testSingle
	| ni oi |
	ni := ObjectSingleNewIs new.
	self assert: (ni is: #is).
	self assert: (ni is: #isNess).
	self deny: (ni is: #isNess1).
	self deny: (ni is: #isNess2).
	self deny: (ni is: #isNess3).
	self deny: (ni is: #isNessSub).
	self deny: (ni is: #noWay).
	oi := ObjectSingleOldIs new.
	self assert: (oi is: #is).
	self assert: (oi is: #isNess).
	self deny: (oi is: #isNess1).
	self deny: (oi is: #isNess2).
	self deny: (oi is: #isNess3).
	self deny: (oi is: #isNessSub).
	self deny: (oi is: #noWay).! !

!LetsGetDownToIsness class methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 19:13'!
memory
	"Not sure how accurate this is"
	| newSpace oldSpace pctChange new old |
	Transcript log: 'Memory used'.
	self oldAndNew do: [ :ea |
		old := ea at: #old.
		new := ea at: #new.
		oldSpace := old spaceUsed.
		newSpace := new spaceUsed.
		pctChange := oldSpace = 0
			ifTrue: [ 0 ]
			ifFalse: [ (newSpace / oldSpace * 100) asInteger ].
		Transcript log: pctChange asString , '% ' , old asString , ': ' , oldSpace asString , ', ' , new asString , ': ' , newSpace asString ].! !

!LetsGetDownToIsness class methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 19:50'!
oldAndNew
	^ OrderedCollection new
		 add:
		(Dictionary new
			
			at: #old
			put: ObjectMultiOldIs;
			
			at: #new
			put: ObjectMultiNewIs;
			 yourself);
		 add:
		(Dictionary new
			
			at: #old
			put: ObjectMultiSubOldIs;
			
			at: #new
			put: ObjectMultiSubNewIs;
			 yourself);
		 add:
		(Dictionary new
			
			at: #old
			put: ObjectNoneOldIs;
			
			at: #new
			put: ObjectNoneNewIs;
			 yourself);
		 add:
		(Dictionary new
			
			at: #old
			put: ObjectRootOldIs;
			
			at: #new
			put: ObjectRootNewIs;
			 yourself);
		 add:
		(Dictionary new
			
			at: #old
			put: ObjectSingleOldIs;
			
			at: #new
			put: ObjectSingleNewIs;
			 yourself);
		 yourself.! !

!LetsGetDownToIsness class methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 19:15'!
tests
	self timing.
	self memory.! !

!LetsGetDownToIsness class methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 19:51'!
timing
	| test timingTests durNew durOld |
	timingTests := OrderedCollection new
		 addAll: ObjectRoot allProtocols;
		 add: #noWay;
		 sort;
		 yourself.
	Transcript log: 'Timing tests'.
	test := [ :obj :val | | start end resultShown |
	Smalltalk garbageCollect.
	resultShown := true.
	start := Time now.
	(1 to: 5000000) do: [ :cnt | | result |
		result := obj is: val.
		resultShown ifFalse: [
			Transcript log: obj asString , ' is: #' , val , ' = ' , result asString.
			resultShown := true ]].
	end := Time now.
	end - start ].
	self oldAndNew do: [ :oan | | oldClass newClass |
		oldClass := oan at: #old.
		newClass := oan at: #new.
		Transcript log: oldClass asString , ' vs. ' , newClass asString.
		timingTests do: [ :ea |
			durOld := test
				value: oldClass new
				value: ea.
			durNew := test
				value: newClass new
				value: ea.
			Transcript log: (durNew / durOld * 100) asInteger asString , '% ' , ea , ' - ' , oldClass asString , ': ' , durOld asString , ', ' , newClass asString , ': ' , durNew asString ]].! !

!ObjectMultiNewIs class methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 19:46'!
protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: self superclass protocols.
		protocols
			 add: #isNess1;
			 add: #isNess2;
			 add: #isNess3 ].
	^ protocols.! !

!ObjectMultiOldIs methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 02:21'!
is: aSymbol
	^ aSymbol == #isNess1
		or: [ aSymbol == #isNess2 ]
		or: [ aSymbol == #isNess3 ]
		or: [ super is: aSymbol ].! !

!ObjectMultiSubNewIs class methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 14:13'!
protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: self superclass protocols.
		protocols
			 add: #isNessSub ].
	^ protocols.! !

!ObjectMultiSubOldIs methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 14:12'!
is: aSymbol
	^ aSymbol == #isNessSub
		or: [ super is: aSymbol ].! !

!ObjectRoot class methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 19:56'!
allProtocols
	| all |
	all := Set new.
	Smalltalk allClasses do: [:cls|
		(cls class includesSelector: #protocols) ifTrue: [
			all addAll: cls protocols]].
	^ all! !

!ObjectRootNewIs methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 19:55'!
is: aSymbol
	self class protocols
		ifNil: [ ^ false ]
		ifNotNil: [ :protocols |
			"^ protocols includes: aSymbol"
			^ protocols array pointsTo: aSymbol ].! !

!ObjectRootNewIs class methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 19:44'!
noteCompilationOf: aSelector meta: isMeta
	aSelector = #protocols ifTrue: [
		Transcript log: 'protocols for ' , self class asString , ' protocols recompiled... setting civar to nil'.
		protocols := nil.
		self allSubclassesDo: [:ea|
			(ea class includesSelector: #protocols) ifTrue: [
				"
			Transcript log: 'firing ', ea asString.
			"
			ea noteCompilationOf: aSelector meta: isMeta.
			] "ifFalse: [
			Transcript log: 'ignoring ', ea asString.
			]"
			]
		"
		"
		].
	super
		noteCompilationOf: aSelector
		meta: isMeta.! !

!ObjectRootNewIs class methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 19:49'!
protocols
	protocols ifNil: [
		protocols := Set new.
		protocols
			 add: #is ].
	^ protocols.! !

!ObjectRootOldIs methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 01:30'!
is: aSymbol
	^ aSymbol == #is or: [ super is: aSymbol ].! !

!ObjectSingleNewIs class methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 14:02'!
protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #isNess ].
	^ protocols.! !

!ObjectSingleOldIs methodsFor: 'as yet unclassified' stamp: 'pb 6/16/2012 01:25'!
is: aSymbol
	^ aSymbol == #isNess or: [ super is: aSymbol ]! !
