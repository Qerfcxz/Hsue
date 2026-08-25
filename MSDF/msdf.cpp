#include "msdf-atlas-gen/msdf-atlas-gen.h"
#include "msdfgen/msdfgen.h"
#include "msdfgen/msdfgen-ext.h"
#include <vector>
#include <cstdlib>
#include <thread>
#include <algorithm>
#include <cstring>
#include <ft2build.h>
#include FT_FREETYPE_H

using namespace msdf_atlas;

extern "C" {

struct MSDF_Glyph {
    int msdf_unicode;
    float msdf_advance;
    float msdf_plane_left;
    float msdf_plane_down;
    float msdf_plane_right;
    float msdf_plane_up;
    float msdf_atlas_left;
    float msdf_atlas_down;
    float msdf_atlas_right;
    float msdf_atlas_up;
};

struct MSDF_Output {
    unsigned char* msdf_pixel;
    int msdf_width;
    int msdf_height;
    float msdf_descent;
    float msdf_ascent;
    MSDF_Glyph* msdf_glyph;
    int msdf_count;
};

__declspec(dllexport) MSDF_Output* MSDF_Generator(const char* font_path,bool exclude_charset,const uint32_t* charset,int charset_length,float font_size,float pixel_range) {
    msdfgen::FreetypeHandle* free_type=msdfgen::initializeFreetype();
    if (!free_type) return nullptr;
    msdfgen::FontHandle* font=msdfgen::loadFont(free_type,font_path);
    if (!font) {
        msdfgen::deinitializeFreetype(free_type);
        return nullptr;
    }
    Charset charset_object;
    if (exclude_charset) {
        FT_Library free_type_library;
        FT_Init_FreeType(&free_type_library);
        FT_Face face;
        if (!FT_New_Face(free_type_library,font_path,0,&face)) {
            std::vector<uint32_t> exclude(charset,charset+charset_length);
            std::sort(exclude.begin(),exclude.end());
            FT_UInt glyph_index;
            FT_ULong char_code=FT_Get_First_Char(face,&glyph_index);
            while (glyph_index!=0) {
                if (!std::binary_search(exclude.begin(),exclude.end(),(uint32_t)char_code)) {
                    charset_object.add(char_code);
                }
                char_code=FT_Get_Next_Char(face,char_code,&glyph_index);
            }
            FT_Done_Face(face);
        }
        FT_Done_FreeType(free_type_library);
    } else {
        for (int index=0;index<charset_length;index++) {
            charset_object.add(charset[index]);
        }
    }
    std::vector<GlyphGeometry> glyphs;
    FontGeometry font_geometry(&glyphs);
    font_geometry.loadCharset(font,1.0,charset_object);
    if (glyphs.empty()) {
        MSDF_Output* result=(MSDF_Output*)malloc(sizeof(MSDF_Output));
        result->msdf_width=0;
        result->msdf_height=0;
        result->msdf_pixel=nullptr;
        result->msdf_count=0;
        result->msdf_ascent=0;
        result->msdf_descent=0;
        result->msdf_glyph=nullptr;
        msdfgen::destroyFont(font);
        msdfgen::deinitializeFreetype(free_type);
        return result;
    }
    TightAtlasPacker packer;
    packer.setDimensionsConstraint(DimensionsConstraint::POWER_OF_TWO_RECTANGLE);
    packer.setScale(font_size);
    packer.setPixelRange(pixel_range);
    packer.pack(glyphs.data(),glyphs.size());
    int width=0,height=0;
    packer.getDimensions(width,height);
    ImmediateAtlasGenerator<float,4,&mtsdfGenerator,BitmapAtlasStorage<msdf_atlas::byte,4>> generator(width,height);
    GeneratorAttributes attributes;
    generator.setAttributes(attributes);
    generator.setThreadCount(std::max(1u,std::thread::hardware_concurrency()));
    generator.generate(glyphs.data(),glyphs.size());
    msdfgen::BitmapConstRef<msdf_atlas::byte,4> bitmap=generator.atlasStorage();
    MSDF_Output* result=(MSDF_Output*)malloc(sizeof(MSDF_Output));
    result->msdf_width=width;
    result->msdf_height=height;
    result->msdf_pixel=(unsigned char*)malloc(width*height*4);
    unsigned char* destination=result->msdf_pixel;
    for (int y=0;y<height;y++) {
        const msdf_atlas::byte* source_row=bitmap(0,height-1-y);
        memcpy(destination,source_row,width*4);
        destination+=width*4;
    }
    const msdfgen::FontMetrics& metrics=font_geometry.getMetrics();
    result->msdf_ascent=(float)metrics.ascenderY;
    result->msdf_descent=(float)metrics.descenderY;
    result->msdf_count=(int)glyphs.size();
    result->msdf_glyph=(MSDF_Glyph*)malloc(sizeof(MSDF_Glyph)*result->msdf_count);
    for (size_t index=0;index<glyphs.size();index++) {
        const auto& single_glyph=glyphs[index];
        MSDF_Glyph& single_msdf_glyph=result->msdf_glyph[index];
        single_msdf_glyph.msdf_unicode=single_glyph.getCodepoint();
        single_msdf_glyph.msdf_advance=(float)single_glyph.getAdvance();
        double plane_left=0,plane_down=0,plane_right=0,plane_up=0;
        single_glyph.getQuadPlaneBounds(plane_left,plane_down,plane_right,plane_up);
        single_msdf_glyph.msdf_plane_left=(float)plane_left;
        single_msdf_glyph.msdf_plane_down=(float)plane_down;
        single_msdf_glyph.msdf_plane_right=(float)plane_right;
        single_msdf_glyph.msdf_plane_up=(float)plane_up;
        double atlas_left=0,atlas_down=height,atlas_right=0,atlas_up=height;
        single_glyph.getQuadAtlasBounds(atlas_left,atlas_down,atlas_right,atlas_up);
        single_msdf_glyph.msdf_atlas_left=(float)atlas_left;
        single_msdf_glyph.msdf_atlas_down=(float)(height-atlas_down);
        single_msdf_glyph.msdf_atlas_right=(float)atlas_right;
        single_msdf_glyph.msdf_atlas_up=(float)(height-atlas_up);
    }
    msdfgen::destroyFont(font);
    msdfgen::deinitializeFreetype(free_type);
    return result;
}

__declspec(dllexport) void MSDF_Cleaner(MSDF_Output* result) {
    if (result) {
        free(result->msdf_pixel);
        free(result->msdf_glyph);
        free(result);
    }
}

}