#ifndef MSDF_H
#define MSDF_H

#include <stdint.h>
#include <stdbool.h>

#ifdef _WIN32
#define EXPORT_API __declspec(dllexport)
#else
#define EXPORT_API __attribute__((visibility("default")))
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    int unicode;
    float advance;
    float plane_left;
    float plane_down;
    float plane_right;
    float plane_up;
    float atlas_left;
    float atlas_down;
    float atlas_right;
    float atlas_up;
} MSDF_Glyph;

typedef struct {
    unsigned char* pixel;
    int width;
    int height;
    float descent;
    float ascent;
    MSDF_Glyph* glyph;
    int count;
} MSDF_Output;

EXPORT_API MSDF_Output* MSDF_Generator(const char* font_path,bool exclude_charset,const uint32_t* charset,int charset_length,float font_size,float pixel_range);

EXPORT_API void MSDF_Cleaner(MSDF_Output* result);

#ifdef __cplusplus
}
#endif

#endif