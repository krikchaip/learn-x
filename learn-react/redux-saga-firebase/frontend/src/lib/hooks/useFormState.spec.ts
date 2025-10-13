import { renderHook, act } from '@testing-library/react-hooks'

import useFormState, { serialize } from './useFormState'

describe('lib/hooks/useFormState', () => {
  const formState = {
    firstname: 'krikchai',
    lastname: 'pongtaveewould'
  }

  it('.get on each field returns a correct value', () => {
    const { result } = renderHook(() => useFormState(formState))

    expect(result.current.firstname.get()).toBe('krikchai')
    expect(result.current.lastname.get()).toBe('pongtaveewould')
  })

  it('.set on each field updates a new value', () => {
    const { result } = renderHook(() => useFormState(formState))

    act(() => {
      result.current.firstname.set('winner')
      result.current.lastname.set('eiei')
    })

    expect(result.current.firstname.get()).toBe('winner')
    expect(result.current.lastname.get()).toBe('eiei')
  })

  it('.update on each field updates a value with function', () => {
    const { result } = renderHook(() => useFormState(formState))

    act(() => {
      result.current.firstname.update(first => `## ${first} ##`)
      result.current.lastname.update(last => `__ ${last} __`)
    })

    expect(result.current.firstname.get()).toBe('## krikchai ##')
    expect(result.current.lastname.get()).toBe('__ pongtaveewould __')
  })

  describe('serialize', () => {
    it('removes all field methods and returns a plain value', () => {
      const { result } = renderHook(() => useFormState(formState))

      const obj = serialize(result.current)

      expect(obj.firstname).toBe('krikchai')
      expect(obj.lastname).toBe('pongtaveewould')
    })
  })
})
