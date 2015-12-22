#include "thorin/util/log.h"

#include "thorin/util/assert.h"

namespace thorin {

bool Log::print_loc_ = true;
Log::Level Log::min_level_ = Log::Info;
std::ostream* Log::stream_ = nullptr;

char Log::level2char(Level level) {
    switch (level) {
        case Error: return 'E';
        case Warn:  return 'W';
        case Info:  return 'I';
        case Debug: return 'D';
    }
    THORIN_UNREACHABLE;
}

}
