'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 26 June 2012 at 7:08:05 pm'!

!classDefinition: 'ActiveModel class' category: #'Kernel-Objects'!
ActiveModel class
	instanceVariableNames: 'protocols'!

!classDefinition: 'Collection class' category: #'Collections-Abstract'!
Collection class
	instanceVariableNames: 'protocols'!

!classDefinition: 'Color class' category: #'Graphics-Primitives'!
Color class
	instanceVariableNames: 'protocols'!

!classDefinition: 'DisplayObject class' category: #'Graphics-Display Objects'!
DisplayObject class
	instanceVariableNames: 'protocols'!

!classDefinition: 'Matrix class' category: #LinearAlgebra!
Matrix class
	instanceVariableNames: 'protocols'!

!classDefinition: 'MessageSend class' category: #'Kernel-Objects'!
MessageSend class
	instanceVariableNames: 'protocols'!

!classDefinition: 'Morph class' category: #'Morphic-Kernel'!
Morph class
	instanceVariableNames: 'protocols'!

!classDefinition: 'MorphicEvent class' category: #'Morphic-Events'!
MorphicEvent class
	instanceVariableNames: 'protocols'!

!classDefinition: 'Stream class' category: #'Collections-Streams'!
Stream class
	instanceVariableNames: 'protocols'!

!classDefinition: 'WeakMessageSend class' category: #'Kernel-Objects'!
WeakMessageSend class
	instanceVariableNames: 'protocols'!

!Object class methodsFor: 'protocols' stamp: 'pb 6/26/2012 17:12'!
                          protocols
	^ Set new.! !

!Object class methodsFor: 'private' stamp: 'pb 6/26/2012 17:12'!
         protocolsReset
	"Subclasses that implement protocols need to create a class inst var that gets set to nil when this method is called"
	self flag: #pbfix.! !

!Object class methodsFor: 'as yet unclassified' stamp: 'pb 6/26/2012 17:12'!
             noteCompilationOf: aSelector meta: isMeta
	(aSelector = #protocols or: aSelector = #protocolsReset) ifTrue: [
		self protocolsReset.
		self allSubclassesDo: [ :ea |
			(ea class includesSelector: #protocols) ifTrue: [
				ea
					noteCompilationOf: aSelector
					meta: isMeta
		 ]]].
	super
		noteCompilationOf: aSelector
		meta: isMeta.! !

!ActiveModel class methodsFor: 'protocols' stamp: 'pb 6/26/2012 20:34'!
                               protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
	].
	^ protocols.! !

!ActiveModel class methodsFor: 'private' stamp: 'pb 6/26/2012 20:34'!
                protocolsReset
	protocols := nil.! !

!Array class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:00'!
      protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #Array.
	].
	^ protocols.! !

!BorderedMorph class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:06'!
              protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #BorderedMorph.
	].
	^ protocols.! !

!CodeProvider class methodsFor: 'protocols' stamp: 'pb 6/26/2012 20:30'!
                      protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #CodeProvider; add: #ShoutEnabled.
	].
	^ protocols.! !

!Collection class methodsFor: 'private' stamp: 'pb 6/26/2012 17:27'!
                           protocolsReset
	protocols := nil.! !

!Collection class methodsFor: 'protocols' stamp: 'pb 6/26/2012 17:24'!
                   protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
	].
	^ protocols.! !

!Color class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:14'!
                              protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #Color.
	].
	^ protocols.! !

!Color class methodsFor: 'private' stamp: 'pb 6/26/2012 18:15'!
                        protocolsReset
	protocols := nil.! !

!ColorForm class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:19'!
                    protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #ColorForm.
	].
	^ protocols.! !

!CompiledMethod class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:50'!
         protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #CompiledMethod.
	].
	^ protocols.! !

!Cursor class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:51'!
  protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #Cursor.
	].
	^ protocols.! !

!DisplayObject class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:17'!
                      protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
	].
	^ protocols.! !

!DisplayObject class methodsFor: 'private' stamp: 'pb 6/26/2012 18:17'!
         protocolsReset
	protocols := nil.! !

!FloatArray class methodsFor: 'protocols' stamp: 'pb 6/26/2012 17:59'!
                   protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #FloatArray.
	].
	^ protocols.! !

!Form class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:20'!
                  protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #Form.
	].
	^ protocols.! !

!HandMorph class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:23'!
                   protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #HandMorph.
	].
	^ protocols.! !

!InnerTextMorph class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:51'!
         protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #InnerTextMorph.
	].
	^ protocols.! !

!LayoutMorph class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:52'!
       protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #LayoutMorph.
	].
	^ protocols.! !

!Matrix class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:53'!
                             protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #Matrix.
	].
	^ protocols.! !

!Matrix class methodsFor: 'private' stamp: 'pb 6/26/2012 18:53'!
                      protocolsReset
	protocols := nil.! !

!MessageSend class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:54'!
                        protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #MessageSend.
	].
	^ protocols.! !

!MessageSend class methodsFor: 'private' stamp: 'pb 6/26/2012 18:54'!
            protocolsReset
	protocols := nil.! !

!Morph class methodsFor: 'private' stamp: 'pb 6/26/2012 18:02'!
protocolsReset
	protocols := nil.! !

!Morph class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:03'!
                        protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #Morph.
	].
	^ protocols.! !

!MorphicEvent class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:09'!
                       protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #MorphicEvent.
	].
	^ protocols.! !

!MorphicEvent class methodsFor: 'private' stamp: 'pb 6/26/2012 18:10'!
          protocolsReset
	protocols := nil.! !

!PluggableScrollPane class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:55'!
                  protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #ScrollPane.
	].
	^ protocols.! !

!PluggableTextModel class methodsFor: 'protocols' stamp: 'pb 6/26/2012 21:27'!
          protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #hasTextProvider; add: #dynamic.
	].
	^ protocols.! !

!Stream class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:58'!
                             protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #Stream.
	].
	^ protocols.! !

!Stream class methodsFor: 'private' stamp: 'pb 6/26/2012 18:58'!
                      protocolsReset
	protocols := nil.! !

!SystemWindow class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:11'!
       protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #SystemWindow.
	].
	^ protocols.! !

!Text class methodsFor: 'protocols' stamp: 'pb 6/26/2012 18:59'!
                protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #Text.
	].
	^ protocols.! !

!WeakMessageSend class methodsFor: 'protocols' stamp: 'pb 6/26/2012 19:00'!
                    protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #MessageSend.
	].
	^ protocols.! !

!WeakMessageSend class methodsFor: 'private' stamp: 'pb 6/26/2012 19:00'!
        protocolsReset
	protocols := nil.! !

!Workspace class methodsFor: 'protocols' stamp: 'pb 6/26/2012 19:01'!
                    protocols
	protocols ifNil: [
		protocols := Set new.
		protocols addAll: superclass protocols.
		protocols add: #ShoutEnabled.
	].
	^ protocols.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'pb 6/26/2012 21:09'!
                allProtocols
		"Protocols that #is: can test for.  Note: for classes that implement a more dynamic #is: (see PluggableTextModel) care should be taken to ensure that there protocols are not defined that don't get listed."
	| all |
	all := Set new.
	Smalltalk allClasses do: [:cls|
		((cls class includesSelector: #protocols) and: ((cls = Object) not)) ifTrue: [
			all addAll: cls protocols]].
	^ all! !

!Object methodsFor: 'testing' stamp: 'pb 6/26/2012 20:39' prior: 16905165!
                      is: aSymbol
	"A means for cleanly replacing isXXX like methods.
	Please use judiciously!!
	Suggested by Igor Stasenko at
	http://lists.squeakfoundation.org/pipermail/squeak-dev/2009-June/136793.html
	aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc.
	
	A few comments:
	
		- Good for kernel tests
		- Good for tests defined in the same package as the receiver
		- Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases
		
		- In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching.
		
		- if a class happens to answer true for several Symbols, consider implementing it like:
			^#(symbol1 symbol2 symbol3) pointsTo: aSymbol
		"
	
	"Enable this to log improper calls to the Transcript..."
	"
	aSymbol class == Symbol ifFalse: [ thisContext sender sender print. aSymbol print ].
	"
	self class protocols
		ifNil: [ ^ false ]
		ifNotNil: [ :protocols |
			^ protocols array pointsTo: aSymbol ].! !

!PluggableTextModel methodsFor: 'testing' stamp: 'pb 6/26/2012 21:28' prior: 16915452!
                        is: aSymbol
	aSymbol == #ShoutEnabled ifTrue: [ ^textProvider is: aSymbol ].
	self class protocols
		ifNil: [ ^ false ]
		ifNotNil: [ :protocols |
			^ protocols array pointsTo: aSymbol ].! !

Array removeSelector: #is:!

BorderedMorph removeSelector: #is:!

ColorForm removeSelector: #is:!

CompiledMethod removeSelector: #is:!

Cursor removeSelector: #is:!

FloatArray removeSelector: #is:!

Form removeSelector: #is:!

HandMorph removeSelector: #is:!

InnerTextMorph removeSelector: #is:!

LayoutMorph removeSelector: #is:!

PluggableScrollPane removeSelector: #is:!

SystemWindow removeSelector: #is:!

Text removeSelector: #is:!

Workspace removeSelector: #is:!

CodeProvider removeSelector: #is:!

Color removeSelector: #is:!

Matrix removeSelector: #is:!

MessageSend removeSelector: #is:!

Morph removeSelector: #is:!

MorphicEvent removeSelector: #is:!

Stream removeSelector: #is:!

WeakMessageSend removeSelector: #is:!