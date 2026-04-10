import 'leaflet/dist/leaflet.css'
import './style.css'
import L from 'leaflet'

/** 画像ピクセル座標に合わせた単純 CRS。外部タイル URL は一切使わない。 */
const W = 1200
const H = 800
const imageBounds = L.latLngBounds([0, 0], [H, W])

const map = L.map('map', {
  crs: L.CRS.Simple,
  minZoom: -2,
  maxZoom: 3,
  zoomControl: true,
  attributionControl: false,
})

L.imageOverlay(`${import.meta.env.BASE_URL}maps/floor-plan.svg`, imageBounds).addTo(map)

const exhibits = [
  { name: '展示室 A', lat: 220, lng: 280, detail: '常設コレクション' },
  { name: 'ロビー', lat: 180, lng: 820, detail: '案内・休憩' },
  { name: '展示室 B', lat: 520, lng: 660, detail: '企画展' },
  { name: 'ショップ', lat: 520, lng: 980, detail: 'ミュージアムショップ' },
]

const layersByName = new Map()

for (const p of exhibits) {
  const m = L.circleMarker([p.lat, p.lng], {
    radius: 10,
    color: '#c9a962',
    weight: 2,
    fillColor: '#e8dcc4',
    fillOpacity: 0.85,
  })
    .addTo(map)
    .bindPopup(`<strong>${p.name}</strong><br>${p.detail}`)
  layersByName.set(p.name, m)
}

/** 1 アイコンで案内パネルを開閉し、一覧から1タップで地図上のポップアップも開く */
function setupGuideFab() {
  const root = document.createElement('div')
  root.className = 'guide-ui'

  const backdrop = document.createElement('div')
  backdrop.className = 'guide-backdrop'
  backdrop.hidden = true

  const panel = document.createElement('div')
  panel.className = 'guide-panel'
  panel.setAttribute('role', 'dialog')
  panel.setAttribute('aria-modal', 'true')
  panel.setAttribute('aria-label', '館内案内')
  panel.hidden = true

  const head = document.createElement('div')
  head.className = 'guide-panel-head'
  head.textContent = '館内案内'

  const list = document.createElement('ul')
  list.className = 'guide-panel-list'
  for (const p of exhibits) {
    const li = document.createElement('li')
    const btn = document.createElement('button')
    btn.type = 'button'
    btn.className = 'guide-spot-btn'
    btn.innerHTML = `<span class="guide-spot-name">${p.name}</span><span class="guide-spot-detail">${p.detail}</span>`
    btn.addEventListener('click', () => {
      const layer = layersByName.get(p.name)
      if (layer) {
        map.setView([p.lat, p.lng], Math.max(map.getZoom(), 0), { animate: true })
        layer.openPopup()
      }
      close()
    })
    li.appendChild(btn)
    list.appendChild(li)
  }

  const fab = document.createElement('button')
  fab.type = 'button'
  fab.className = 'guide-fab'
  fab.setAttribute('aria-haspopup', 'dialog')
  fab.setAttribute('aria-expanded', 'false')
  fab.setAttribute('aria-label', '館内案内を表示')
  fab.innerHTML = `
    <svg width="26" height="26" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true">
      <circle cx="12" cy="12" r="10"/>
      <path d="M12 16v-4M12 8h.01"/>
    </svg>`

  function open() {
    backdrop.hidden = false
    panel.hidden = false
    fab.setAttribute('aria-expanded', 'true')
    fab.setAttribute('aria-label', '案内を閉じる')
    fab.classList.add('is-open')
  }

  function close() {
    backdrop.hidden = true
    panel.hidden = true
    fab.setAttribute('aria-expanded', 'false')
    fab.setAttribute('aria-label', '館内案内を表示')
    fab.classList.remove('is-open')
  }

  function toggle() {
    if (panel.hidden) open()
    else close()
  }

  fab.addEventListener('click', () => toggle())
  backdrop.addEventListener('click', () => close())

  document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape' && !panel.hidden) {
      e.preventDefault()
      close()
      fab.focus()
    }
  })

  panel.append(head, list)
  root.append(backdrop, panel, fab)
  document.body.appendChild(root)
}

setupGuideFab()

map.fitBounds(imageBounds)

function syncMapSize() {
  map.invalidateSize()
  map.fitBounds(imageBounds)
}

requestAnimationFrame(syncMapSize)
window.addEventListener('resize', syncMapSize)
