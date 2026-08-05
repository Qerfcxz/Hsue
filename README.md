# Hsue 🧊

**[English](#english) | [中文](#中文)**

---

<a id="english"></a>
# Hsue (English)

> A hardcore, purely functional 2D UI engine built from scratch with Haskell and the modern SDL3 GPU API.

⚠️ **WARNING: HIGHLY EXPERIMENTAL & RAPID ITERATION PHASE** ⚠️
> **Hsue is currently under heavy development.** The architecture and APIs are changing rapidly on a daily basis. **Stability is NOT guaranteed at this stage**, and backwards compatibility is not a priority. Expect crashes, panics, and breaking changes. Use at your own risk!

## 🚀 Overview

Hsue is an exploration of how to build a high-performance, modern UI framework using purely functional paradigms. Instead of relying on legacy SDL2, Hsue directly embraces the emerging **SDL3 GPU API** (abstracting Vulkan/D3D12/Metal) and loads pre-compiled DXIL shaders to explore the potential of hardware-accelerated UI rendering.

## ✨ Key Features

*   **Modern GPU Pipeline & Automatic Batching:** Built on top of the SDL3 GPU API. Features Shader Storage Buffer Objects (SSBOs) for primitive parameters, custom HLSL pixel-shader clipping, and an integrated `Collector` pipeline that automatically batches draw calls by texture atlas.
*   **Bytecode-Compiled Functional Coroutines:** A custom, purely functional Coroutine DSL supporting advanced semantics (`Wait`, `Fork`, `Race`, `Case`, `Dynamic_clone`, `Pause`, `Skip`, `Assign`, etc.). It compiles down to a bytecode-like linear instruction stream (`Linear_coroutine`) executed via ST-Monad state machines with isolated variable memory layouts.
*   **Off-Screen Canvas & Programmable Shaders:** Introduces `Canvas` for native Render-to-Texture (RTT) capabilities, allowing nested UI sub-trees to be drawn into isolated FBOs. Paired with a custom shader pipeline (`Shader_canvas`), it enables developers to apply custom HLSL fragment shaders and dynamic Uniform buffer injections for advanced post-processing effects, all orchestrated within the pure functional data flow.
*   **MSDF Text Rendering & Automatic Typesetting:** Native support for Multi-channel Signed Distance Field (MSDF) text. Interfaces directly with `msdf-atlas-gen` to track dynamic charsets, manage glyph atlases, and perform multi-line paragraph typesetting (`do_typesetting`) for crisp text at any scale.
*   **Declarative Selector System:** A powerful query and mutation mechanism (`Selector a`) for traversing and modifying complex nested widget trees (`Group`, `Vector`, `Coroutine`, `Trigger`, etc.) using combinators, pattern matching, and monadic updates.
*   **Deferred Request Queue:** Side effects and state mutations are strictly isolated. Widgets emit explicit `Request` intentions (`Render`, `Create_widget`, `Clean_atlas`, `Unlock`, `Load_charset`), which the engine processes sequentially to maintain a pure functional data flow.
*   **"Projection" Caching & Adaptive Windowing:** Snapshot caching for the UI tree (`Projection a = Without object | With object image`) freezes hierarchical matrix transforms to minimize redundant traversals. Paired with aspect-ratio-preserving adaptive window scaling (`adaptive_window`).
*   **Dynamic Atlas & Animation Subsystem:** Includes a built-in BSP rectangle packing packer for dynamic texture atlases, alongside native support for multi-frame animated sprites via `SDL_image` (`IMG_LoadAnimation`).

---

<a id="中文"></a>
# Hsue (中文)

> 一个使用 Haskell 与最新 SDL3 GPU API 从零构建的硬核、纯函数式 2D UI 引擎。

⚠️ **警告：极度实验性 & 快速迭代中** ⚠️
> **Hsue 目前正处于高频更新的快速迭代阶段。** 引擎架构和底层 API 随时可能发生破坏性更改。**目前暂不考虑任何稳定性和向后兼容性**，程序可能会出现崩溃或未定义行为。请绝对不要用于生产环境！

## 🚀 简介

Hsue 旨在探索如何用纯函数式编程的理念，构建一个现代、高性能的 UI 框架。Hsue 抛弃了老旧的 SDL2 后端，直接拥抱新兴的 **SDL3 GPU API**（对齐 Vulkan/D3D12/Metal 等图形管线），并配合预编译的 DXIL 着色器，以探索硬件加速 UI 渲染的潜力。

## ✨ 核心特性

*   **现代 GPU 渲染与 Draw Call 自动合批:** 基于 SDL3 GPU 接口搭建。结合 HLSL 片段着色器与 Shader Storage Buffer (SSBO) 传输图元参数，并内置 `Collector` 收集器管线，根据纹理 Atlas 自动完成 Draw Call 的分组与合批。
*   **字节码编译型纯函数协程:** 高阶纯函数协程 DSL，支持复杂的并发与状态控制（如 `Wait`, `Fork`, `Race`, `Case`, `Dynamic_clone`, `Pause`, `Skip`, `Assign` 等）。底层会编译为类似字节码的线性指令流 (`Linear_coroutine`)，并在 ST Monad 中高效运行状态机与隔离变量内存。
*   **离屏缓冲 (Canvas) 与可编程后期着色器:** 提供原生的 Render-to-Texture (RTT) 能力，允许将深层嵌套的 UI 子树独立渲染至 `Canvas` 纹理。结合自定义着色器管线 (`Shader_canvas`)，开发者可对 Canvas 目标应用自定义的 HLSL 片段着色器，并通过动态 Uniform 数据注入，在纯函数控制流中实现各种高级图像后期处理与视觉特效。
*   **MSDF 高清文本与排版引擎:** 原生集成 MSDF（多通道有符号距离场）渲染技术。底层对接 `msdf-atlas-gen` C API，实现动态字符集提取、自动段落/多行排版计算 (`do_typesetting`) 以及任意缩放级别下的平滑矢量文本显示。
*   **声明式 Selector 节点检索系统:** 提供强大的 `Selector a` 组合子体系，支持对深层嵌套的 Widget 结构（如 `Group`, `Vector`, `Coroutine`, `Trigger` 等）进行高效的结构遍历、模式匹配与 Monadic 局部状态更新。
*   **延迟请求队列 (Request System):** 严格隔离副作用与状态变更。Widget 仅需发送 `Request` 意图（如渲染、节点增删、图集重建、字符集加载等），由引擎主循环统一消费队列，保持清晰单向的数据流。
*   **“投影”缓存与自适应窗口:** UI 树快照机制 (`Projection`) 能将父节点的仿射变换计算“冻结”并缓存，避免跨帧重复计算；配合设计分辨率自适应缩放逻辑 (`adaptive_window`)，轻松应对窗口拉伸与分辨率变化。
*   **动态图集与动画子系统:** 内置基于二叉空间分割（BSP）的矩形打包算法，运行时自动合并小图；同时原生支持基于 `SDL_image` 的多帧 Sequence 动态图集与动画播放 (`Animation`)。
