MADINCDIR=-I../include -I/usr/local/include  -I/usr/include -I/usr/local/include/boost-1_35
MADLIBDIR=-L/usr/local/lib -L/usr/lib -L../lib
H=../include
OBJ=../obj
LIB=../lib
CFLAGS= -pg -W -Wall -Winline -pedantic $(MADINCDIR) -g \
	-O2 --param max-inline-insns-single=1000 \
	--param large-function-growth=1500\
	--param inline-unit-growth=1000
LDFLAGS=$(MADINCDIR)  -pg -g -DPA_NO_DS -DPA_NO_ASIO -DPA_NO_WDMKS 
CC=g++

LOADLIBES=-lm -lmad -lSDLmain \
	${LIB}/libSDL_gfxx.a -lportaudio -lwinmm -olel32 -lstdc++ -luuid -ldl \
 	${LIB}/libSDL_ttf.a -lfreetype -lz \
	${LIB}/libleaktrace.a\
	-lSDL
BOOST=../lib/boost

LDLIBS=$(LOADLIBES)

.PHONY: all
../Karaoke.exe: $(OBJ)/Karaoke.o $(OBJ)/Errors.o $(OBJ)/Exceptions.o\
		$(OBJ)/StackTrace.o $(OBJ)/Timer.o \
		$(OBJ)/ScreenManager.o $(OBJ)/GameEngine.o \
		$(OBJ)/MP3Decoder.o $(OBJ)/Graphics.o \
		$(OBJ)/LoadingCallback.o $(OBJ)/LoadingScreen.o \
		$(OBJ)/IntroScreen.o $(OBJ)/MainMenuScreen.o \
		$(OBJ)/SingScreen.o $(OBJ)/SongResultScreen.o \
		$(OBJ)/Note.o $(OBJ)/Syllable.o $(OBJ)/Sentence.o \
		$(OBJ)/NoteAnalyzer.o $(OBJ)/Player.o \
		$(OBJ)/MyStringUtils.o $(OBJ)/Preferences.o \
		$(OBJ)/AudioUtils.o $(OBJ)/Audio.o \
		$(OBJ)/AudioSamples.o $(OBJ)/RingBuffer.o \
		$(OBJ)/Screen.o $(OBJ)/PPScreen.o\
		$(OBJ)/Oscilloscope.o $(OBJ)/MemoryInfo.o \
		$(OBJ)/Tests.o $(OBJ)/Font.o $(OBJ)/Version.o \
		$(OBJ)/Surface.o $(OBJ)/Widget.o $(OBJ)/MemoryWidget.o \
		$(OBJ)/Metric.o $(OBJ)/Graph.o $(OBJ)/DotGraph.o \
		$(OBJ)/SongSelectionScreen.o $(OBJ)/Directory.o \
		$(OBJ)/SongList.o $(OBJ)/Song.o \
		$(OBJ)/SongComparator.o $(OBJ)/RecordedData.o \
		$(OBJ)/TimeBarWidget.o $(OBJ)/LyricsWidget.o \
		$(OBJ)/StaveWidget.o $(OBJ)/ErrorsWidget.o \
		$(OBJ)/BarGraph.o $(OBJ)/AudioCircularBuffer.o \
		$(OBJ)/Spectrogram.o $(OBJ)/CompoundSpectrumAnalyzer.o \
		$(OBJ)/YGradientColorFunction.o $(OBJ)/SimpleSpectrumAnalyzer.o \
		$(OBJ)/FFTSpectrumAnalyzer.o $(OBJ)/MyMath.o \
		$(OBJ)/SpectrumAnalyzer.o $(OBJ)/GameDataPointers.o \
		${OBJ}/Timer.o ${OBJ}/NoteAnalyzer.o\
		${BOOST}/error_code.o \
		${BOOST}/operations.o \
		${BOOST}/path.o \
		${BOOST}/portability.o \
		${BOOST}/utf8_codecvt_facet.o
	$(CC) $(LDFLAGS) $(LIB)/libleaktrace.a $^ $(LOADLIBES) -o $@
	cat Karaoke.ver | awk '{ if ( $$2 == "BUILD" ) print $$1 " " \
	$$2 " " $$3+1; else print $$0  };'>Karaoke.ver2
	rm Karaoke.ver
	mv Karaoke.ver2 Karaoke.ver
	rm $(OBJ)/Version.o

$(OBJ)/Tests.o: Tests.cpp $(H)/Tests.h
	$(CC) $(CFLAGS) Tests.cpp -o $@ -c

$(OBJ)/Font.o: Font.cpp $(H)/Font.h
	$(CC) $(CFLAGS) Font.cpp -o $@ -c
	
$(OBJ)/MemoryInfo.o: MemoryInfo.cpp $(H)/MemoryInfo.h
	$(CC) $(CFLAGS) MemoryInfo.cpp -o $@ -c

$(OBJ)/Karaoke.o: Karaoke.cpp $(H)/Errors.h $(H)/Exceptions.h $(H)/MyAssert.h
	$(CC) $(CFLAGS) Karaoke.cpp -o $@ -c
	
$(OBJ)/Errors.o: Errors.cpp $(H)/Errors.h $(H)/Exceptions.h $(H)/MyAssert.h
	$(CC) $(CFLAGS) Errors.cpp -o $@ -c
	
$(OBJ)/Exceptions.o: Exceptions.cpp $(H)/Exceptions.h
	$(CC) $(CFLAGS) Exceptions.cpp -o $@ -c
	
$(OBJ)/StackTrace.o: StackTrace.cpp $(H)/StackTrace.h
	$(CC) $(CFLAGS) StackTrace.cpp -o $@ -c
	

$(OBJ)/GameEngine.o: GameEngine.cpp $(H)/GameEngine.h $(H)/MyAssert.h
	$(CC) $(CFLAGS) GameEngine.cpp -o $@ -c
	
$(OBJ)/ScreenManager.o: ScreenManager.cpp $(H)/ScreenManager.h
	$(CC) $(CFLAGS) ScreenManager.cpp -o $@ -c
	
$(OBJ)/LoadingCallback.o: LoadingCallback.cpp $(H)/LoadingCallback.h
	$(CC) $(CFLAGS) LoadingCallback.cpp -o $@ -c

$(OBJ)/LoadingScreen.o: LoadingScreen.cpp $(H)/LoadingScreen.h
	$(CC) $(CFLAGS) LoadingScreen.cpp -o $@ -c

$(OBJ)/IntroScreen.o: IntroScreen.cpp $(H)/IntroScreen.h
	$(CC) $(CFLAGS) IntroScreen.cpp -o $@ -c
	
$(OBJ)/MP3Decoder.o:	MP3Decoder.cpp $(H)/MP3Decoder.h
	$(CC) $(CFLAGS) MP3Decoder.cpp -o $@ -c
	
$(OBJ)/MainMenuScreen.o: MainMenuScreen.cpp $(H)/MainMenuScreen.h
	$(CC) $(CFLAGS) MainMenuScreen.cpp -o $@ -c
	
$(OBJ)/SingScreen.o: SingScreen.cpp $(H)/SingScreen.h
	$(CC) $(CFLAGS) SingScreen.cpp -o $@ -c

$(OBJ)/SongResultScreen.o: SongResultScreen.cpp $(H)/SongResultScreen.h
	$(CC) $(CFLAGS) SongResultScreen.cpp -o $@ -c

$(OBJ)/Sentence.o: Sentence.cpp $(H)/Sentence.h
	$(CC) $(CFLAGS) Sentence.cpp -o $@ -c
	
$(OBJ)/Note.o: Note.cpp $(H)/Note.h
	$(CC) $(CFLAGS) Note.cpp -o $@ -c
	
$(OBJ)/Syllable.o: Syllable.cpp $(H)/Syllable.h
	$(CC) $(CFLAGS) Syllable.cpp -o $@ -c


$(OBJ)/Player.o: Player.cpp $(H)/Player.h
	$(CC) $(CFLAGS) Player.cpp -o $@ -c

$(OBJ)/Graphics.o: Graphics.cpp $(H)/Graphics.h
	$(CC) $(CFLAGS) Graphics.cpp -o $@ -c

$(OBJ)/Preferences.o: Preferences.cpp $(H)/Preferences.h
	$(CC) $(CFLAGS) Preferences.cpp -o $@ -c

$(OBJ)/MyStringUtils.o: MyStringUtils.cpp $(H)/MyStringUtils.h
	$(CC) $(CFLAGS) MyStringUtils.cpp -o $@ -c

$(OBJ)/Audio.o: Audio.cpp $(H)/Audio.h
	$(CC) $(CFLAGS) Audio.cpp -o $@ -c
	
$(OBJ)/AudioUtils.o: AudioUtils.cpp $(H)/AudioUtils.h
	$(CC) $(CFLAGS) AudioUtils.cpp -o $@ -c

$(OBJ)/AudioSamples.o: AudioSamples.cpp $(H)/AudioSamples.h
	$(CC) $(CFLAGS) AudioSamples.cpp -o $@ -c

$(OBJ)/RingBuffer.o: RingBuffer.cpp $(H)/RingBuffer.h $(H)/MyAssert.h
	$(CC) $(CFLAGS) RingBuffer.cpp -o $@ -c

$(OBJ)/Screen.o: Screen.cpp $(H)/Screen.h
	$(CC) $(CFLAGS) Screen.cpp -o $@ -c	

$(OBJ)/PPScreen.o: PPScreen.cpp $(H)/PPScreen.h
	$(CC) $(CFLAGS) PPScreen.cpp -o $@ -c	

$(OBJ)/Oscilloscope.o: Oscilloscope.cpp $(H)/Oscilloscope.h
	$(CC) $(CFLAGS) Oscilloscope.cpp -o $@ -c	

$(OBJ)/Version.o: Version.cpp $(H)/Version.h
	$(CC) $(CFLAGS) Version.cpp -o $@ -c	

$(OBJ)/Surface.o: Surface.cpp $(H)/Surface.h
	$(CC) $(CFLAGS) Surface.cpp -o $@ -c	

$(OBJ)/Widget.o: Widget.cpp $(H)/Widget.h
	$(CC) $(CFLAGS) Widget.cpp -o $@ -c	

$(OBJ)/MemoryWidget.o: MemoryWidget.cpp $(H)/MemoryWidget.h
	$(CC) $(CFLAGS) MemoryWidget.cpp -o $@ -c	

$(OBJ)/Metric.o: Metric.cpp $(H)/Metric.h
	$(CC) $(CFLAGS) Metric.cpp -o $@ -c

$(OBJ)/Graph.o: Graph.cpp $(H)/Graph.h
	$(CC) $(CFLAGS) Graph.cpp -o $@ -c

$(OBJ)/DotGraph.o: DotGraph.cpp $(H)/DotGraph.h
	$(CC) $(CFLAGS) DotGraph.cpp -o $@ -c


$(OBJ)/SongSelectionScreen.o: SongSelectionScreen.cpp $(H)/SongSelectionScreen.h
	$(CC) $(CFLAGS) SongSelectionScreen.cpp -o $@ -c


$(OBJ)/Directory.o: Directory.cpp $(H)/Directory.h
	$(CC) $(CFLAGS) Directory.cpp -o $@ -c


$(OBJ)/SongList.o: SongList.cpp $(H)/SongList.h
	$(CC) $(CFLAGS) SongList.cpp -o $@ -c


$(OBJ)/Song.o: Song.cpp $(H)/Song.h
	$(CC) $(CFLAGS) Song.cpp -o $@ -c

$(OBJ)/SongComparator.o: SongComparator.cpp $(H)/SongComparator.h
	$(CC) $(CFLAGS) SongComparator.cpp -o $@ -c

$(OBJ)/RecordedData.o: RecordedData.cpp $(H)/RecordedData.h
	$(CC) $(CFLAGS) RecordedData.cpp -o $@ -c

$(OBJ)/TimeBarWidget.o: TimeBarWidget.cpp $(H)/TimeBarWidget.h
	$(CC) $(CFLAGS) TimeBarWidget.cpp -o $@ -c

$(OBJ)/LyricsWidget.o: LyricsWidget.cpp $(H)/LyricsWidget.h
	$(CC) $(CFLAGS) LyricsWidget.cpp -o $@ -c

$(OBJ)/StaveWidget.o: StaveWidget.cpp $(H)/StaveWidget.h
	$(CC) $(CFLAGS) StaveWidget.cpp -o $@ -c

$(OBJ)/ErrorsWidget.o: ErrorsWidget.cpp $(H)/ErrorsWidget.h
	$(CC) $(CFLAGS) ErrorsWidget.cpp -o $@ -c


$(OBJ)/BarGraph.o: BarGraph.cpp $(H)/BarGraph.h $(H)/MyAssert.h
	$(CC) $(CFLAGS) BarGraph.cpp -o $@ -c


$(OBJ)/Spectrogram.o: Spectrogram.cpp $(H)/Spectrogram.h
	$(CC) $(CFLAGS) Spectrogram.cpp -o $@ -c


$(OBJ)/AudioCircularBuffer.o: AudioCircularBuffer.cpp $(H)/AudioCircularBuffer.h
	$(CC) $(CFLAGS) AudioCircularBuffer.cpp -o $@ -c

$(OBJ)/FFTSpectrumAnalyzer.o: FFTSpectrumAnalyzer.cpp $(H)/FFTSpectrumAnalyzer.h
	$(CC) $(CFLAGS) FFTSpectrumAnalyzer.cpp -o $@ -c

$(OBJ)/CompoundSpectrumAnalyzer.o: CompoundSpectrumAnalyzer.cpp $(H)/CompoundSpectrumAnalyzer.h
	$(CC) $(CFLAGS) CompoundSpectrumAnalyzer.cpp -o $@ -c

$(OBJ)/YGradientColorFunction.o: YGradientColorFunction.cpp $(H)/YGradientColorFunction.h $(H)/ColorFunction.h
	$(CC) $(CFLAGS) YGradientColorFunction.cpp -o $@ -c

$(OBJ)/SimpleSpectrumAnalyzer.o: SimpleSpectrumAnalyzer.cpp $(H)/SimpleSpectrumAnalyzer.h
	$(CC) $(CFLAGS) SimpleSpectrumAnalyzer.cpp -o $@ -c

$(OBJ)/MyMath.o: MyMath.cpp $(H)/MyMath.h
	$(CC) $(CFLAGS) MyMath.cpp -o $@ -c

$(OBJ)/SpectrumAnalyzer.o: SpectrumAnalyzer.cpp $(H)/SpectrumAnalyzer.h
	$(CC) $(CFLAGS) SpectrumAnalyzer.cpp -o $@ -c

$(OBJ)/GameDataPointers.o: GameDataPointers.cpp $(H)/GameDataPointers.h
	$(CC) $(CFLAGS) GameDataPointers.cpp -o $@ -c

$(OBJ)/Themes.o: Themes.cpp $(H)/Themes.h
	$(CC) $(CFLAGS) Themes.cpp -o $@ -c

$(OBJ)/Timer.o: Timer.cpp $(H)/Timer.h
	$(CC) $(CFLAGS) Timer.cpp -o $@ -c

$(OBJ)/NoteAnalyzer.o: NoteAnalyzer.cpp $(H)/NoteAnalyzer.h
	$(CC) $(CFLAGS) NoteAnalyzer.cpp -o $@ -c

.PHONY: clear
clear:
	rm $(OBJ)/*.o; rm ../*.exe
