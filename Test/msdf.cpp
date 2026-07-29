#include "msdf-atlas-gen/msdf-atlas-gen.h"
#include "msdfgen/msdfgen.h"
#include "msdfgen/msdfgen-ext.h"
#include <vector>
#include <cstdlib>
#include <thread>
#include <algorithm>
#include <cstring>

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

__declspec(dllexport) MSDF_Output* MSDF_Generator(
    const char* fontPath,
    const uint32_t* charset,
    int charsetLen,
    float fontSize,
    float pxRange)
{
    msdfgen::FreetypeHandle* ft = msdfgen::initializeFreetype();
    if (!ft) return nullptr;
    msdfgen::FontHandle* font = msdfgen::loadFont(ft, fontPath);
    if (!font) { msdfgen::deinitializeFreetype(ft); return nullptr; }
    msdfgen::FontMetrics rawMetrics;
    msdfgen::getFontMetrics(rawMetrics, font);
    double fontScale = rawMetrics.emSize > 0 ? 1.0 / rawMetrics.emSize : 1.0;
    Charset charsetObj;
    for (int i = 0; i < charsetLen; i++) charsetObj.add(charset[i]);
    std::vector<GlyphGeometry> glyphs;
    FontGeometry fontGeometry(&glyphs);
    fontGeometry.loadCharset(font, 1.0, charsetObj); 
    TightAtlasPacker packer;
    packer.setDimensionsConstraint(DimensionsConstraint::POWER_OF_TWO_RECTANGLE);
    packer.setScale(fontSize); 
    packer.setPixelRange(pxRange);
    packer.pack(glyphs.data(), glyphs.size());
    int width = 0, height = 0;
    packer.getDimensions(width, height);
    ImmediateAtlasGenerator<float, 4, &mtsdfGenerator, BitmapAtlasStorage<msdf_atlas::byte, 4>> generator(width, height);
    GeneratorAttributes attributes;
    generator.setAttributes(attributes);
    generator.setThreadCount(std::max(1u, std::thread::hardware_concurrency())); 
    generator.generate(glyphs.data(), glyphs.size());
    msdfgen::BitmapConstRef<msdf_atlas::byte, 4> bitmap = generator.atlasStorage();
    MSDF_Output* res = (MSDF_Output*)malloc(sizeof(MSDF_Output));
    res->msdf_width = width;
    res->msdf_height = height;
    res->msdf_pixel = (unsigned char*)malloc(width * height * 4);
    unsigned char* dst = res->msdf_pixel;
    for (int y = 0; y < height; y++) {
        const msdf_atlas::byte* srcRow = bitmap(0, height - 1 - y);
        memcpy(dst, srcRow, width * 4);
        dst += width * 4;
    }
    const msdfgen::FontMetrics& metrics = fontGeometry.getMetrics();
    res->msdf_ascent = (float)metrics.ascenderY;
    res->msdf_descent = (float)metrics.descenderY;
    res->msdf_count = (int)glyphs.size();
    res->msdf_glyph = (MSDF_Glyph*)malloc(sizeof(MSDF_Glyph) * res->msdf_count);
    for (size_t i = 0; i < glyphs.size(); i++) {
        const auto& g = glyphs[i];
        MSDF_Glyph& cg = res->msdf_glyph[i];
        cg.msdf_unicode = g.getCodepoint();
        cg.msdf_advance = (float)g.getAdvance();
        double pl = 0, pb = 0, pr = 0, pt = 0;
        g.getQuadPlaneBounds(pl, pb, pr, pt);
        cg.msdf_plane_left  = (float)pl;
        cg.msdf_plane_down  = (float)pb; 
        cg.msdf_plane_right = (float)pr;
        cg.msdf_plane_up    = (float)pt; 
        double al = 0, ab = height, ar = 0, at = height;
        g.getQuadAtlasBounds(al, ab, ar, at);
        cg.msdf_atlas_left  = (float)al;
        cg.msdf_atlas_down  = (float)(height - ab);
        cg.msdf_atlas_right = (float)ar;
        cg.msdf_atlas_up    = (float)(height - at);
    }
    msdfgen::destroyFont(font);
    msdfgen::deinitializeFreetype(ft);
    return res;
}

__declspec(dllexport) void MSDF_Cleaner(MSDF_Output* res) {
    if (res) {
        free(res->msdf_pixel);
        free(res->msdf_glyph);
        free(res);
    }
}

}