# Hsue 🧊

**[English](#english) | [中文](#中文)**

---

<a id="english"></a>
# Hsue (English)

> A hardcore, purely functional 2D UI engine built from scratch with Haskell and the modern SDL3 GPU API.

⚠️ **WARNING: HIGHLY EXPERIMENTAL & RAPID ITERATION PHASE** ⚠️
> **Hsue is currently under heavy development.** The architecture and APIs are changing rapidly on a daily basis. **Stability is NOT guaranteed at this stage**, and backwards compatibility is not a priority. Expect crashes, panics, and breaking changes. Use at your own risk!

## 🚀 Overview

Hsue is an exploration of how to build a high-performance, modern UI framework using purely functional paradigms. Instead of relying on legacy OpenGL or Cairo, Hsue directly embraces the bleeding-edge **SDL3 GPU API** (abstracting Vulkan/D3D12/Metal) with DXIL shader compilation to achieve maximum rendering efficiency.

## ✨ Key Features

*   **Modern GPU Rendering:** Built on top of the SDL3 GPU pipeline. Automatically batches draw calls and manages GPU buffers seamlessly.
*   **Deferred Request System:** Side effects and state mutations are completely isolated. Widgets emit `Request`s (e.g., `Render`, `Create_widget`) to a central queue, which the engine processes sequentially, maintaining a pure functional data flow.
*   **Smart "Projection" System:** A unique snapshot and caching mechanism (`Projection a = Without object | With object image`). It caches the visual results of hierarchical transforms (filters, matrices) from parent to child, reducing $O(Depth)$ tree-traversals to $O(1)$ lookups during rendering.
*   **Dynamic Texture Atlas:** Includes a built-in, BSP-based automatic texture packer. Small images are packed into dynamic atlases on the fly to minimize context switching and texture binding overhead.
*   **Active vs. Inactive Segregation:** The UI tree is structurally divided into `Active` nodes (handling logic, triggers, events) and `Inactive` nodes (pure visual collectors). This guarantees zero-overhead hit-testing for static UI elements.

---

<a id="中文"></a>
# Hsue (中文)

> 一个使用 Haskell 与最新 SDL3 GPU API 从零构建的硬核、纯函数式 2D UI 引擎。

⚠️ **警告：极度实验性 & 快速迭代中** ⚠️
> **Hsue 目前正处于高频更新的快速迭代阶段。** 引擎架构和底层 API 随时可能发生破坏性更改。**目前暂不考虑任何稳定性和向后兼容性**，程序可能会出现崩溃或未定义行为。请绝对不要用于生产环境！

## 🚀 简介

Hsue 旨在探索如何用纯函数式编程的理念，构建一个现代、高性能的 UI 框架。Hsue 抛弃了老旧的 OpenGL 或 Cairo 后端，直接拥抱最前沿的 **SDL3 GPU API**（对齐 Vulkan/D3D12/Metal 等现代图形管线），并原生支持 DXIL 编译着色器，从底层释放硬件渲染潜力。

## ✨ 核心特性

*   **现代 GPU 渲染管线:** 基于 SDL3 GPU 接口构建。底层支持自动 Draw Call 合并和高效的 GPU 顶点/索引 Buffer 管理。
*   **延迟请求队列 (Request System):** 完美的纯函数状态管理。组件不直接产生副作用，而是向引擎发出 `Request` 意图（如渲染、创建窗口、定时器），引擎在主循环中统一消费队列，彻底避免竞态条件。
*   **独创的“投影”系统 (Projection):** 一套绝妙的 UI 树快照与缓存机制。通过区分本体与影像（`Without` / `With image`），引擎能将父节点的仿射变换或视觉滤镜“冻结”并缓存，将复杂的树状变换开销从 $O(Depth)$ 降为 $O(1)$。
*   **动态纹理图集 (Dynamic Atlas):** 内置基于二叉空间分割（BSP）的矩形打包算法，运行时自动将零碎小图合并为大尺寸 Texture Atlas，极大优化渲染性能。
*   **动静分离 UI 树:** 引擎在底层将 UI 组件严格拆分为交互层 (`Active`) 与纯视觉层 (`Inactive`)。静态组件不再参与鼠标事件的命中测试遍历，彻底消除冗余的性能损耗。

---
