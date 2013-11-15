# Try to find Thorin library and include path.
# Once done this will define
#
# THORIN_FOUND
# THORIN_INCLUDE_DIR
# THORIN_LIBRARIES (including dependencies to LLVM/WFV2)
# THORIN_LIBRARIES_DIR

SET ( PROJ_NAME THORIN )

FIND_PACKAGE ( LLVM QUIET )

FIND_PATH ( THORIN_ROOT_DIR thorin-config.cmake PATHS ${THORIN_DIR} $ENV{THORIN_DIR} $ENV{AYDSL2_ROOT} )
SET ( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${THORIN_ROOT_DIR} )

SET ( THORIN_OUTPUT_LIBS thorin.lib thorin.so thorin.dll libthorin libthorin.so libthorin.a libthorin.dll )

FIND_PATH ( THORIN_INCLUDE_DIR NAMES thorin/world.h PATHS ${THORIN_ROOT_DIR}/src ${THORIN_ROOT_DIR}/include ${THORIN_ROOT_DIR}/build/include )
FIND_PATH ( THORIN_LIBRARIES_DIR
	NAMES
		${THORIN_OUTPUT_LIBS}
	PATHS
		${THORIN_ROOT_DIR}/build
		${THORIN_ROOT_DIR}/build_debug
		${THORIN_ROOT_DIR}/build_release
		${THORIN_ROOT_DIR}/lib
		${THORIN_ROOT_DIR}/build/lib
	PATH_SUFFIXES
		${CMAKE_CONFIGURATION_TYPES}
)

# include anydsl specific stuff
INCLUDE ( ${CMAKE_CURRENT_LIST_DIR}/thorin-shared.cmake )

IF ( THORIN_LIBRARIES_DIR )
	FIND_LIBRARY ( THORIN_LIBRARIES_DEBUG NAMES ${THORIN_OUTPUT_LIBS} PATHS ${THORIN_LIBRARIES_DIR} PATH_SUFFIXES Debug)
	FIND_LIBRARY ( THORIN_LIBRARIES_RELEASE NAMES ${THORIN_OUTPUT_LIBS} PATHS ${THORIN_LIBRARIES_DIR} PATH_SUFFIXES Release )
	SET ( THORIN_LIBRARIES
		optimized ${THORIN_LIBRARIES_RELEASE}
		debug ${THORIN_LIBRARIES_DEBUG}
	)
	# get the dependencies
	get_thorin ( THORIN_TEMP_LIBRARIES )
	SET ( THORIN_LIBRARIES ${THORIN_TEMP_LIBRARIES} ${THORIN_LIBRARIES} )
ENDIF()

IF ( THORIN_INCLUDE_DIR AND THORIN_LIBRARIES )
	SET ( THORIN_FOUND TRUE CACHE BOOL "" FORCE )
ELSE()
	SET ( THORIN_FOUND FALSE CACHE BOOL "" FORCE )
ENDIF()

MARK_AS_ADVANCED ( THORIN_FOUND )