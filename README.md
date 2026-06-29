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

*   **Modern GPU Rendering:** Built on top of the SDL3 GPU pipeline. Features fundamental draw call batching, adaptive window resizing, and explicit GPU buffer management.
*   **Functional Coroutine Subsystem:** A custom, purely functional Coroutine DSL (`Wait`, `Fork`, `Emit`, `While`, `Repeat`, etc.). It compiles down to bytecode-like linear instructions (`Linear_coroutine`), making complex UI state machines, animations, and parallelism declarative and robust.
*   **MSDF Crisp Text Rendering:** First-class support for Multi-channel Signed Distance Field (MSDF) text. Automatically interfaces with `msdf-atlas-gen` to generate, cache, and manage dynamic charsets and glyph atlases, ensuring pixel-perfect text at any scale.
*   **Deferred Request System:** Side effects and state mutations are explicitly isolated. Widgets emit `Request`s (e.g., `Render`, `Create_widget`, `Load_charset`) to a central queue, which the engine processes sequentially, maintaining a clear functional data flow.
*   **"Projection" Caching System:** A snapshot mechanism for the UI tree (`Projection a = Without object | With object image`). It caches the visual results of hierarchical transforms, reducing redundant affine matrix calculations and traversals during rendering.
*   **Dynamic Texture Atlas:** Includes a built-in, BSP-based texture packer. Small images are packed into dynamic atlases on the fly to reduce texture binding overhead.
*   **Composable Event Triggers:** Evolved into a highly compositional `Widget` system. Instead of rigid structures, the engine gracefully handles logic via `Trigger`, `Mix_trigger`, and `Group` wrappers, isolating side-effects (`Io_trigger`) directly alongside the pure UI nodes.

---

<a id="中文"></a>
# Hsue (中文)

> 一个使用 Haskell 与最新 SDL3 GPU API 从零构建的硬核、纯函数式 2D UI 引擎。

⚠️ **警告：极度实验性 & 快速迭代中** ⚠️
> **Hsue 目前正处于高频更新的快速迭代阶段。** 引擎架构和底层 API 随时可能发生破坏性更改。**目前暂不考虑任何稳定性和向后兼容性**，程序可能会出现崩溃或未定义行为。请绝对不要用于生产环境！

## 🚀 简介

Hsue 旨在探索如何用纯函数式编程的理念，构建一个现代、高性能的 UI 框架。Hsue 抛弃了老旧的 SDL2 后端，直接拥抱新兴的 **SDL3 GPU API**（对齐 Vulkan/D3D12/Metal 等图形管线），并配合预编译的 DXIL 着色器，以探索硬件加速 UI 渲染的潜力。

## ✨ 核心特性

*   **现代 GPU 渲染管线:** 基于 SDL3 GPU 接口构建。底层包含了基础的 Draw Call 合批逻辑、自适应窗口缩放逻辑以及显式的 GPU Buffer 管理。
*   **纯函数式协程系统:** 引擎内置了一套极为完备的纯函数协程 DSL（包含 `Wait`, `Fork`, `Emit`, `Repeat` 等语义）。它在底层会被编译为类似字节码的线性指令 (`Linear_coroutine`)，使得复杂的 UI 动画、状态机与高并发逻辑能够以声明式的方式优雅编写。
*   **MSDF 高清文本渲染:** 原生集成多通道有符号距离场 (MSDF) 文本渲染引擎。底层自动调用 `msdf-atlas-gen` 管理动态字符集与图集缓存，并利用片段着色器实现了任意缩放级别下的平滑、无损文本显示。
*   **延迟请求队列 (Request System):** 明确分离了副作用与状态变更。组件不直接执行副作用，而是向引擎发出 `Request` 意图（如渲染、加载字体、创建组件等），引擎在主循环中统一消费队列，保持清晰的数据流。
*   **“投影”缓存系统 (Projection):** 一套 UI 树快照机制。通过区分本体与影像（`Without` / `With image`），引擎能将父节点的仿射变换等结果“冻结”并缓存，避免每帧重复计算复杂的树状结构开销。
*   **动态纹理图集 (Dynamic Atlas):** 内置基于二叉空间分割（BSP）的矩形打包算法，运行时自动将零碎小图合并为大尺寸 Texture Atlas，有效降低渲染时的纹理绑定开销。
*   **高可组合的触发器组件 (Triggers):** 演进出了一套高组合性的 `Widget` 体系。不再使用僵硬的结构约束，而是通过 `Trigger`, `Mix_trigger`, `Group` 等节点进行事件响应与包装，并将副作用（`Io_trigger`）安全地隔离在纯 UI 节点之旁。
